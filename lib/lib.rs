#![no_std]
#![allow(clippy::items_after_test_module)]

extern crate alloc;
pub use hashbrown;

pub struct Options<'a, H: HtmlTags> {
    pub html_tags: &'a H,
    pub label_prefix: &'a str,
    pub label_use_prefix: &'a str,
    pub heading_labels: HeadingLabels,
    pub flags: Flags,
    pub h1_level: i8,
}

pub trait HtmlTags {
    fn get(&self, tag: &[u8]) -> Option<(&[u8], HtmlTagKind)>;
}

pub type HtmlTagMap<'a, H = DefaultHashBuilder> =
    HashMap<CaseInsensitiveKey<&'a [u8]>, HtmlTagKind, H>;

impl<H: BuildHasher> HtmlTags for HtmlTagMap<'_, H> {
    fn get(&self, tag: &[u8]) -> Option<(&[u8], HtmlTagKind)> {
        let (k, &v) = self.get_key_value(&CaseInsensitive(tag))?;
        Some((k.0.0, v))
    }
}

#[derive(Clone, Copy)]
pub enum HtmlTagKind {
    Void,
    RawText,
    EscapableRawText,
    Normal,
}

#[derive(Clone, Copy)]
pub enum HeadingLabels {
    GitHub,
    Jupyter,
}

impl HeadingLabels {
    pub fn from_u8(byte: u8) -> Option<Self> {
        Some(match byte {
            0 => Self::GitHub,
            1 => Self::Jupyter,
            _ => return None,
        })
    }
}

bitflags! {
    #[repr(transparent)]
    pub struct Flags: u8 {
        const SMART_PUNCTUATION = 0b0000_0001;
        const RAW_TYPST = 0b0000_0010;
        const MATH = 0b0000_0100;
    }
}

pub fn run<H: HtmlTags>(markdown: &str, options: Options<'_, H>) -> Result<Vec<u8>, String> {
    let mut markdown_options = pulldown_cmark::Options::empty()
        | pulldown_cmark::Options::ENABLE_STRIKETHROUGH
        | pulldown_cmark::Options::ENABLE_TABLES
        | pulldown_cmark::Options::ENABLE_FOOTNOTES;
    if options.flags.contains(Flags::SMART_PUNCTUATION) {
        markdown_options |= pulldown_cmark::Options::ENABLE_SMART_PUNCTUATION;
    }
    if options.flags.contains(Flags::MATH) {
        markdown_options |= pulldown_cmark::Options::ENABLE_MATH;
    }

    let mut result = Vec::new();
    let mut open_tags: Vec<Vec<CaseInsensitive<&[u8]>>> = vec![Vec::new()];
    let start_scope = |open_tags: &mut Vec<Vec<CaseInsensitive<&[u8]>>>, result: &mut Vec<u8>| {
        open_tags.push(Vec::new());
        result.extend_from_slice(b"[");
    };
    let close_tags = |open_tags: &mut Vec<Vec<CaseInsensitive<&[u8]>>>, result: &mut Vec<u8>| {
        for _ in open_tags.pop().unwrap() {
            result.extend_from_slice(b"]");
        }
    };
    let end_scope = |open_tags: &mut Vec<Vec<CaseInsensitive<&[u8]>>>, result: &mut Vec<u8>| {
        close_tags(open_tags, result);
        result.extend_from_slice(b"]");
    };
    let mut current_header_label = None::<Label>;
    let mut label_tracker = LabelTracker {
        counts: HashMap::new(),
        prefix: options.label_prefix,
    };
    let mut footnotes = Footnotes::default();
    let mut farm = Farm::default();

    let mut parser = Unpeekable::new(pulldown_cmark::Parser::new_with_broken_link_callback(
        markdown,
        markdown_options,
        Some(BrokenLinkCallback),
    ));

    while let (Some(event), _) = parser.next() {
        use pulldown_cmark::CodeBlockKind;
        use pulldown_cmark::{Event as E, Tag, TagEnd};
        match event {
            E::Start(Tag::Paragraph) => {}
            E::End(TagEnd::Paragraph) => result.extend_from_slice(b"\n\n"),

            E::Start(Tag::Heading {
                level,
                id: _,
                classes: _,
                attrs: _,
            }) => {
                let heading_level = level as i16 - 1 + i16::from(options.h1_level);
                match heading_level {
                    ..=-1 => {}
                    0 => {
                        result.extend_from_slice(b"#set document(title:");
                        start_scope(&mut open_tags, &mut result);
                    }
                    n @ 1.. => {
                        result.push(b'\n');
                        result.resize(result.len() + (n as usize), b'=');
                        result.push(b' ');
                    }
                }

                // Nested headings are impossible
                assert!(current_header_label.is_none());

                if heading_level > 0 {
                    current_header_label = Some(Label::default());
                }
            }
            E::End(TagEnd::Heading(level)) => {
                let heading_level = level as i16 - 1 + i16::from(options.h1_level);

                if let Some(label) = current_header_label.take() {
                    label_tracker.write_after_heading(label, &mut result);
                }

                match heading_level {
                    ..=-1 => {}
                    0 => {
                        end_scope(&mut open_tags, &mut result);
                        result.extend_from_slice(b");#title();");
                    }
                    1.. => result.extend_from_slice(b"\n"),
                }
            }

            E::Start(Tag::BlockQuote(_)) => {
                result.extend_from_slice(b"#quote(block:true)");
                start_scope(&mut open_tags, &mut result);
            }
            E::End(TagEnd::BlockQuote(_)) => {
                end_scope(&mut open_tags, &mut result);
                result.extend_from_slice(b"\n\n");
            }

            E::Start(Tag::Strikethrough) => {
                result.extend_from_slice(b"#strike");
                start_scope(&mut open_tags, &mut result);
            }
            E::End(TagEnd::Strikethrough) => {
                end_scope(&mut open_tags, &mut result);
                result.push(b';');
            }

            E::Start(Tag::CodeBlock(kind)) => {
                result.extend_from_slice(b"#raw(block:true,");
                if let CodeBlockKind::Fenced(lang) = kind
                    && !lang.is_empty()
                {
                    result.extend_from_slice(b"lang:\"");
                    escape_string(lang.as_bytes(), &mut result);
                    result.extend_from_slice(b"\",");
                }
                result.push(b'"');
                loop {
                    match parser.next().0.unwrap() {
                        E::Text(text) => escape_string(text.as_bytes(), &mut result),
                        E::End(TagEnd::CodeBlock) => break,
                        other => return Err(format!("unexpected {other:?} in code block")),
                    }
                }
                result.extend_from_slice(b"\")\n");
            }
            E::End(TagEnd::CodeBlock) => unreachable!(),

            E::Start(Tag::List(first)) => {
                if let Some(first) = first {
                    result.extend_from_slice(b"#enum(start:");
                    result.extend_from_slice(itoa::Buffer::new().format(first).as_bytes());
                    result.extend_from_slice(b",");
                } else {
                    result.extend_from_slice(b"#list(");
                }
            }
            E::End(TagEnd::List(_)) => result.extend_from_slice(b")\n"),
            E::Start(Tag::Item) => start_scope(&mut open_tags, &mut result),
            E::End(TagEnd::Item) => {
                end_scope(&mut open_tags, &mut result);
                result.extend_from_slice(b",");
            }

            E::Start(Tag::Table(alignments)) => {
                result.extend_from_slice(b"#table(align:(");
                for align in &alignments {
                    result.extend_from_slice(match align {
                        Alignment::Left => b"left,",
                        Alignment::Right => b"right,",
                        Alignment::Center => b"center,",
                        Alignment::None => b"auto,",
                    });
                }
                result.extend_from_slice(b"),columns:");
                let mut columns = itoa::Buffer::new();
                result.extend_from_slice(columns.format(alignments.len()).as_bytes());
                result.extend_from_slice(b",");
            }
            E::End(TagEnd::Table) => result.extend_from_slice(b");"),
            E::Start(Tag::TableHead) => result.extend_from_slice(b"table.header("),
            E::End(TagEnd::TableHead) => result.extend_from_slice(b"),"),

            E::Start(Tag::TableRow) => {}
            E::End(TagEnd::TableRow) => {}

            E::Start(Tag::TableCell) => start_scope(&mut open_tags, &mut result),
            E::End(TagEnd::TableCell) => {
                end_scope(&mut open_tags, &mut result);
                result.extend_from_slice(b",");
            }

            E::Start(Tag::Emphasis) => {
                result.extend_from_slice(b"#emph");
                start_scope(&mut open_tags, &mut result);
            }
            E::End(TagEnd::Emphasis) => {
                end_scope(&mut open_tags, &mut result);
                result.push(b';');
            }

            E::Start(Tag::Strong) => {
                result.extend_from_slice(b"#strong");
                start_scope(&mut open_tags, &mut result)
            }
            E::End(TagEnd::Strong) => {
                end_scope(&mut open_tags, &mut result);
                result.push(b';');
            }

            E::Start(Tag::Link {
                link_type:
                    pulldown_cmark::LinkType::CollapsedUnknown
                    | pulldown_cmark::LinkType::ShortcutUnknown,
                dest_url,
                title: _,
                id: _,
            }) => {
                serialize(&mut parser, |_, _| {});
                assert_eq!(parser.next().0, Some(E::End(TagEnd::Link)));

                let label = dest_url.strip_prefix("@").unwrap();
                result.extend_from_slice(b"#ref(label(\"");
                escape_string(options.label_use_prefix.as_bytes(), &mut result);
                escape_string(label.as_bytes(), &mut result);
                result.extend_from_slice(b"\"));");
            }

            E::Start(Tag::Link {
                link_type: pulldown_cmark::LinkType::ReferenceUnknown,
                dest_url,
                title: _,
                id: _,
            }) => {
                let label = dest_url.strip_prefix("@").unwrap();
                // Since we don't get whether it's a broken link on `TagEnd`, we use an IIFE to
                // allow the ending syntax to be the same for both @ and regular links.
                result.extend_from_slice(b"#(s=>ref(label(\"");
                escape_string(options.label_use_prefix.as_bytes(), &mut result);
                escape_string(label.as_bytes(), &mut result);
                result.extend_from_slice(b"\"),supplement:s))");
                start_scope(&mut open_tags, &mut result);
            }
            E::Start(Tag::Link {
                link_type: _,
                dest_url,
                title: _,
                id: _,
            }) => {
                result.extend_from_slice(b"#link(");
                if let Some(label) = dest_url.strip_prefix("#") {
                    result.extend_from_slice(b"label(\"");
                    escape_string(options.label_use_prefix.as_bytes(), &mut result);
                    escape_string(label.as_bytes(), &mut result);
                    result.extend_from_slice(b"\")");
                } else {
                    result.extend_from_slice(b"\"");
                    escape_string(dest_url.as_bytes(), &mut result);
                    result.extend_from_slice(b"\"");
                }
                result.extend_from_slice(b")");
                start_scope(&mut open_tags, &mut result);
            }
            E::End(TagEnd::Link) => {
                end_scope(&mut open_tags, &mut result);
                result.push(b';');
            }

            E::Start(Tag::Image {
                link_type: _,
                dest_url,
                title: _,
                id: _,
            }) => {
                result.extend_from_slice(b"#image(\"");
                escape_string(dest_url.as_bytes(), &mut result);
                result.extend_from_slice(b"\",alt:\"");
                serialize(&mut parser, |s, _| escape_string(s.as_bytes(), &mut result));
                assert_eq!(parser.next().0, Some(E::End(TagEnd::Image)));
                result.extend_from_slice(b"\");");
            }
            E::End(TagEnd::Image) => unreachable!(),

            E::Start(Tag::FootnoteDefinition(label)) => {
                footnotes.begin_define(label, mem::take(&mut result));
                open_tags.push(Vec::new());
            }
            E::End(TagEnd::FootnoteDefinition) => {
                close_tags(&mut open_tags, &mut result);
                footnotes.end_define(&mut result);
            }

            E::Start(
                Tag::DefinitionList
                | Tag::DefinitionListDefinition
                | Tag::DefinitionListTitle
                | Tag::MetadataBlock(_)
                | Tag::Superscript
                | Tag::Subscript,
            ) => todo!(),

            E::End(
                TagEnd::DefinitionList
                | TagEnd::DefinitionListDefinition
                | TagEnd::DefinitionListTitle
                | TagEnd::MetadataBlock(_)
                | TagEnd::Superscript
                | TagEnd::Subscript,
            ) => todo!(),

            E::Start(Tag::HtmlBlock) | E::End(TagEnd::HtmlBlock) => {}

            E::Text(text) => {
                escape_text(text.as_bytes(), &mut result);
                if let Some(label) = current_header_label.as_mut() {
                    label.push(&text, options.heading_labels);
                }
            }

            E::Code(code) => {
                result.extend_from_slice(b"#raw(block:false,\"");
                escape_string(code.as_bytes(), &mut result);
                result.extend_from_slice(b"\");");

                if let Some(label) = current_header_label.as_mut() {
                    label.push(&code, options.heading_labels);
                }
            }

            E::Html(s) | E::InlineHtml(s) => {
                farm.clear();
                let mut cx = HtmlContext {
                    farm: &farm,
                    html: s.as_bytes(),
                    parser: &mut parser,
                    tags: options.html_tags,
                    open_tags: open_tags.last_mut().unwrap(),
                    result: &mut result,
                    raw_typst: options.flags.contains(Flags::RAW_TYPST),
                    // We don't need to pass in `current_header_label`, because text only occurs in
                    // HTML inside HTML blocks, while headers only contain inline HTML.
                };
                parse_html(&mut cx);
            }

            E::InlineMath(s) => {
                // We use #inlinemath(`…`) for inline math
                result.extend_from_slice(b"#inlinemath(\"");
                escape_string(s.as_bytes(), &mut result);
                result.extend_from_slice(b"\");");

                if let Some(label) = current_header_label.as_mut() {
                    label.push(&s, options.heading_labels);
                }
            }

            E::DisplayMath(s) => {
                // We use #displaymath(`…`) for display math
                result.extend_from_slice(b"#displaymath(\"");
                escape_string(s.as_bytes(), &mut result);
                result.extend_from_slice(b"\");");
            }

            E::SoftBreak => result.push(b' '),
            E::HardBreak => result.extend_from_slice(b"\\ "),

            E::Rule => result.extend_from_slice(b"#rule()\n"),

            E::FootnoteReference(label) => footnotes.reference(label, result.len()),

            E::TaskListMarker(_) => unreachable!(),
        }
    }
    // We don’t pass in `label_use_prefix` because it’s rare to want to reference a footnote
    // defined in one document in a different document.
    let mut new_result = Vec::new();
    footnotes.fill_in_definitions(&result, &mut new_result, options.label_prefix);
    result = new_result;

    // To allow using cmarker inline, we (rather crudely) remove paragraph breaks from the end.
    while result.ends_with(b"\n") {
        result.pop();
    }

    close_tags(&mut open_tags, &mut result);
    assert_eq!(open_tags.len(), 0);

    Ok(result)
}

struct BrokenLinkCallback;

impl<'input> pulldown_cmark::BrokenLinkCallback<'input> for BrokenLinkCallback {
    fn handle_broken_link(
        &mut self,
        link: pulldown_cmark::BrokenLink<'input>,
    ) -> Option<(CowStr<'input>, CowStr<'input>)> {
        if link.reference.starts_with("@") {
            return Some((link.reference, CowStr::from("")));
        }
        None
    }
}

#[derive(Default)]
struct Footnotes<'a> {
    /// Insertion points of the currently active context, which is either the main content or an
    /// in-progress footnote definition.
    ///
    /// We go back over these and insert `#footnote`s or references once we have all the
    /// definitions.
    insertion_points: Vec<(usize, CowStr<'a>)>,
    /// All completed footnote definitions and their insertion points.
    definitions: HashMap<CowStr<'a>, FootnoteDefinition<'a>>,
    /// Stack of footnotes being defined (since footnote definitions may be nested). For each one,
    /// contains the in-progress footnote definition that should be returned to.
    open_definitions: Vec<(CowStr<'a>, FootnoteDefinition<'a>)>,
}

struct FootnoteDefinition<'a> {
    content: Vec<u8>,
    insertion_points: Vec<(usize, CowStr<'a>)>,
}

impl<'a> Footnotes<'a> {
    fn reference(&mut self, footnote: CowStr<'a>, index: usize) {
        self.insertion_points.push((index, footnote));
    }
    fn begin_define(&mut self, footnote: CowStr<'a>, content: Vec<u8>) {
        self.open_definitions.push((
            footnote,
            FootnoteDefinition {
                content,
                insertion_points: mem::take(&mut self.insertion_points),
            },
        ));
    }
    fn end_define(&mut self, result: &mut Vec<u8>) {
        let (footnote, previous_definition) = self.open_definitions.pop().unwrap();
        self.definitions.insert(
            footnote,
            FootnoteDefinition {
                content: mem::take(result),
                insertion_points: mem::take(&mut self.insertion_points),
            },
        );
        FootnoteDefinition {
            content: *result,
            insertion_points: self.insertion_points,
        } = previous_definition;
    }
    fn fill_in_definitions(&mut self, mut content: &[u8], out: &mut Vec<u8>, label_prefix: &str) {
        assert!(self.open_definitions.is_empty());

        let mut prev_i = 0;
        for (i, footnote) in mem::take(&mut self.insertion_points) {
            let (before, after) = content.split_at(i.strict_sub(prev_i));
            out.extend_from_slice(before);
            content = after;
            prev_i = i;

            match self.definitions.remove(&footnote) {
                Some(FootnoteDefinition {
                    content,
                    insertion_points,
                }) => {
                    self.insertion_points = insertion_points;
                    out.extend_from_slice(b"#footnote[");
                    self.fill_in_definitions(&content, out, label_prefix);
                    out.extend_from_slice(b"]#label(\"");
                    escape_string(label_prefix.as_bytes(), out);
                    escape_string(footnote.as_bytes(), out);
                    out.extend_from_slice(b"\");");
                }
                None => {
                    out.extend_from_slice(b"#ref(label(\"");
                    escape_string(label_prefix.as_bytes(), out);
                    escape_string(footnote.as_bytes(), out);
                    out.extend_from_slice(b"\"));");
                }
            }
        }
        out.extend_from_slice(content);
    }
}

struct HtmlContext<'a, 'input, T: HtmlTags> {
    farm: &'a Farm,
    html: &'a [u8],
    parser: &'a mut Unpeekable<pulldown_cmark::Parser<'input, BrokenLinkCallback>>,
    tags: &'input T,
    open_tags: &'a mut Vec<CaseInsensitive<&'input [u8]>>,
    result: &'a mut Vec<u8>,
    raw_typst: bool,
}

fn parse_html(cx: &mut HtmlContext<'_, '_, impl HtmlTags>) {
    while let Some(i) = memchr2(b'<', b'&', cx.html) {
        let (before, after) = cx.html.split_at(i);
        escape_text(before, cx.result);
        cx.html = &after[1..];
        match after[0] {
            b'<' if html_open_tag(cx).is_some() => {}
            b'<' if html_close_tag(cx).is_some() => {}
            b'<' if html_comment(cx).is_some() => {}
            b'<' => cx.result.extend_from_slice(b"\\<"),
            b'&' => match decode_character_reference_escape(&mut cx.html) {
                Some(s) => {
                    cx.result.extend_from_slice(b"#(\"");
                    cx.result.extend_from_slice(s.as_bytes());
                    cx.result.extend_from_slice(b"\");");
                }
                None => cx.result.extend_from_slice(b"&"),
            },
            _ => unreachable!(),
        }
    }
    escape_text(cx.html, cx.result);
}

fn html_open_tag(cx: &mut HtmlContext<'_, '_, impl HtmlTags>) -> Option<()> {
    let result = VecTransaction::new(cx.result);

    if !cx.html.first()?.is_ascii_alphabetic() {
        return None;
    }

    let tag_name_len = cx
        .html
        .iter()
        .take_while(|&&b| b.is_ascii_alphanumeric() || b == b'-')
        .count();
    let (source_tag_name, mut rest) = cx.html.split_at(tag_name_len);

    let tag = cx.tags.get(source_tag_name);
    if let Some((tag_name, _)) = tag {
        result.inner.extend_from_slice(b"#(html.");
        result.inner.extend_from_slice(tag_name);
        result.inner.extend_from_slice(b")((");
    }
    let mut attribute_names = HashSet::new();

    loop {
        rest = rest.trim_ascii_start();
        match rest.first()? {
            c if c.is_ascii_alphabetic() || b"_:".contains(c) => {
                let attribute_name_len = rest
                    .iter()
                    .take_while(|b| b.is_ascii_alphanumeric() || b"_.:-".contains(b))
                    .count();
                let (attribute_name, new_rest) = rest.split_at(attribute_name_len);
                rest = new_rest.trim_ascii_start();

                let attribute_value = match rest {
                    [b'=', after_equals @ ..] => {
                        rest = after_equals.trim_ascii_start();
                        match rest {
                            [] | [b'=' | b'<' | b'>' | b'`', ..] => return None,
                            &[delim @ (b'"' | b'\''), ref after_delim @ ..] => {
                                let (value, new_rest) =
                                    after_delim.split_at(memchr(delim, after_delim)?);
                                rest = &new_rest[1..];
                                value
                            }
                            [_, ..] => {
                                let attribute_value_len = rest
                                    .iter()
                                    .take_while(|&b| {
                                        !b.is_ascii_whitespace() && !b"\"\'=<>`".contains(b)
                                    })
                                    .count();
                                let (value, new_rest) = rest.split_at(attribute_value_len);
                                rest = new_rest;
                                value
                            }
                        }
                    }
                    _ => b"",
                };

                if attribute_names.insert(attribute_name) && tag.is_some() {
                    result.inner.push(b'"');
                    escape_string(attribute_name, result.inner);
                    result.inner.extend_from_slice(b"\":\"");
                    escape_string_with_character_references(attribute_value, result.inner);
                    result.inner.extend_from_slice(b"\",");
                }
            }
            b'>' => {
                rest = &rest[1..];
                break;
            }
            b'/' => {
                rest = rest.strip_prefix(b"/>")?;
                break;
            }
            _ => return None,
        }
    }

    result.commit();
    cx.html = rest;

    if let Some((tag_name, tag_kind)) = tag {
        if attribute_names.is_empty() {
            cx.result.push(b':');
        }
        cx.result.extend_from_slice(b")");
        match tag_kind {
            HtmlTagKind::Void => cx.result.extend_from_slice(b");"),
            HtmlTagKind::Normal => {
                cx.open_tags.push(CaseInsensitive(tag_name));
                cx.result.extend_from_slice(b")[");
            }
            HtmlTagKind::RawText | HtmlTagKind::EscapableRawText => {
                cx.result.extend_from_slice(b",\"");
                let maybe_decode: fn(&[u8], &mut Vec<u8>) = match tag_kind {
                    HtmlTagKind::EscapableRawText => escape_string_with_character_references,
                    HtmlTagKind::RawText => escape_string,
                    _ => unreachable!(),
                };

                'outer: loop {
                    if cx.html.is_empty() {
                        let Some(html) = serialize_until_html(cx.parser, |text, is_html| {
                            if is_html {
                                maybe_decode(text.as_bytes(), cx.result)
                            } else {
                                escape_string(text.as_bytes(), cx.result)
                            }
                        }) else {
                            break;
                        };
                        cx.html = cx.farm.add(html).as_bytes();
                    }
                    for possible_end in memmem::find_iter(cx.html, b"</") {
                        let (before, after) = cx.html.split_at(possible_end);
                        if let Some(rest) = after[2..]
                            .split_at_checked(tag_name.len())
                            .filter(|&(s, _)| CaseInsensitive(s) == CaseInsensitive(tag_name))
                            .and_then(|(_, rest)| rest.trim_ascii_start().strip_prefix(b">"))
                        {
                            cx.html = rest;
                            maybe_decode(before, cx.result);
                            break 'outer;
                        }
                    }
                    maybe_decode(mem::take(&mut cx.html), cx.result);
                }
                cx.result.extend_from_slice(b"\");");
            }
        }
    }

    Some(())
}

fn html_close_tag(cx: &mut HtmlContext<'_, '_, impl HtmlTags>) -> Option<()> {
    let [b'/', rest @ ..] = cx.html else {
        return None;
    };
    if !rest.first()?.is_ascii_alphabetic() {
        return None;
    }

    let tag_name_len = rest
        .iter()
        .take_while(|&&b| b.is_ascii_alphanumeric() || b == b'-')
        .count();
    let (tag_name, rest) = rest.split_at(tag_name_len);
    let tag_name = CaseInsensitive(tag_name);
    cx.html = rest.trim_ascii_start().strip_prefix(b">")?;

    if let Some(found) = cx.open_tags.iter().rposition(|&t| t == tag_name) {
        for _ in 0..cx.open_tags.len() - found {
            cx.result.extend_from_slice(b"];");
        }
        cx.open_tags.truncate(found);
    }

    Some(())
}

fn html_comment(cx: &mut HtmlContext<'_, '_, impl HtmlTags>) -> Option<()> {
    cx.html = cx.html.strip_prefix(b"!--")?;

    const BEGIN_EXCLUDE: &[u8] = b"typst-begin-exclude-->";
    const END_EXCLUDE: &[u8] = b"<!--typst-end-exclude-->";
    if cx.html.starts_with(BEGIN_EXCLUDE) {
        cx.html = loop {
            if let Some(end) = memmem::find(cx.html, END_EXCLUDE) {
                break &cx.html[end + END_EXCLUDE.len()..];
            }
            let Some(html) = serialize_until_html(cx.parser, |_, _| {}) else {
                break b"";
            };
            cx.html = cx.farm.add(html).as_bytes();
        };
        return Some(());
    }

    let raw_typst = match (cx.raw_typst, cx.html.strip_prefix(b"raw-typst")) {
        (true, Some(rest)) => {
            cx.html = rest;
            true
        }
        _ => false,
    };

    loop {
        if cx.html.is_empty() {
            use pulldown_cmark::Event as E;
            match cx.parser.next() {
                (Some(E::Text(text)), _) => {
                    if raw_typst {
                        cx.result.extend_from_slice(text.as_bytes());
                    }
                    continue;
                }
                (Some(E::Html(s) | E::InlineHtml(s)), _) => cx.html = cx.farm.add(s).as_bytes(),
                (other, unpeeker) => {
                    unpeeker.unpeek(other);
                    break;
                }
            }
        }

        if let Some(end) = memmem::find(cx.html, b"-->") {
            if raw_typst {
                cx.result.extend_from_slice(&cx.html[..end]);
            }
            cx.html = &cx.html[end + b"-->".len()..];
            break;
        }
        if raw_typst {
            cx.result.extend_from_slice(cx.html);
        }
        cx.html = b"";
    }

    Some(())
}

/// Best-effort attempt to serialize events as a string.
///
/// The callback is called with `true` if the argument is HTML.
fn serialize<'input>(
    parser: &mut Unpeekable<pulldown_cmark::Parser<'input, BrokenLinkCallback>>,
    mut f: impl FnMut(CowStr<'input>, bool),
) {
    while let Some(html) = serialize_until_html(parser, &mut f) {
        f(html, true)
    }
}

/// Best-effort attempt to serialize events as a string.
///
/// Returns `Some` if top-level HTML is encountered.
/// The callback is called with `true` if the argument is HTML.
fn serialize_until_html<'input>(
    parser: &mut Unpeekable<pulldown_cmark::Parser<'input, BrokenLinkCallback>>,
    mut f: impl FnMut(CowStr<'input>, bool),
) -> Option<CowStr<'input>> {
    let mut layers = 0_u32;
    loop {
        use pulldown_cmark::{Event as E, Tag, TagEnd};
        let (event, unpeeker) = parser.next();
        match event? {
            E::Html(s) | E::InlineHtml(s) if layers == 0 => return Some(s),
            E::Html(s) | E::InlineHtml(s) => f(s, true),
            E::Text(s) | E::Code(s) | E::InlineMath(s) | E::DisplayMath(s) => f(s, false),
            E::SoftBreak => f(CowStr::Borrowed(" "), false),
            E::HardBreak => f(CowStr::Borrowed("\n"), false),
            E::Start(Tag::Paragraph | Tag::HtmlBlock) => {}
            E::End(TagEnd::HtmlBlock) => {}
            E::End(TagEnd::Paragraph) => f(CowStr::Borrowed("\n\n"), false),
            E::Start(_) => layers += 1,
            event @ E::End(_) if layers == 0 => {
                unpeeker.unpeek(Some(event));
                return None;
            }
            E::End(_) => layers -= 1,
            _ => {}
        }
    }
}

/// Utility type for case-insensitive comparisons.
#[derive(Clone, Copy)]
pub struct CaseInsensitive<T>(pub T);

impl<T: AsRef<[u8]>> PartialEq for CaseInsensitive<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref().eq_ignore_ascii_case(other.0.as_ref())
    }
}

impl<T: AsRef<[u8]>> Eq for CaseInsensitive<T> {}

impl<T: AsRef<[u8]>> Hash for CaseInsensitive<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for byte in self.0.as_ref().iter().map(u8::to_ascii_lowercase) {
            hasher.write_u8(byte);
        }
        hasher.write_u8(0xFF);
    }
}

impl<T: AsRef<[u8]>> AsRef<[u8]> for CaseInsensitive<T> {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

/// A variant of `CaseInsensitive<T>` to be used as hash map keys.
///
/// Because of the blanket implementation of `impl<T> Borrow<T> for T`,
/// we have `impl<'a> Borrow<CaseInsensitive<&'a [u8]>> for CaseInsensitive<&'a [u8]>`
/// but cannot `impl<'short, 'long: 'short> Borrow<CaseInsensitive<&'short [u8]>> for CaseInsensitive<&'long [u8]>`.
/// We solve this by using a different type for the keys.
pub type CaseInsensitiveKey<T> = CaseInsensitive<CaseInsensitive<T>>;

impl<'short, 'long: 'short> Borrow<CaseInsensitive<&'short [u8]>>
    for CaseInsensitiveKey<&'long [u8]>
{
    fn borrow(&self) -> &CaseInsensitive<&'short [u8]> {
        &self.0
    }
}

mod vec_transaction {
    pub(crate) struct VecTransaction<'a, T> {
        pub inner: &'a mut Vec<T>,
        original_len: usize,
    }

    impl<'a, T> VecTransaction<'a, T> {
        pub(crate) fn new(inner: &'a mut Vec<T>) -> Self {
            Self {
                original_len: inner.len(),
                inner,
            }
        }
        pub(crate) fn commit(self) {
            mem::forget(self);
        }
    }

    impl<T> Drop for VecTransaction<'_, T> {
        fn drop(&mut self) {
            self.inner.truncate(self.original_len);
        }
    }

    use alloc::vec::Vec;
    use core::mem;
}
use vec_transaction::VecTransaction;

/// Polyfill of this ACP: https://github.com/rust-lang/libs-team/issues/557
mod unpeekable {
    pub(crate) struct Unpeekable<I: Iterator> {
        next: Option<I::Item>,
        iter: I,
    }

    impl<I: Iterator> Unpeekable<I> {
        pub(crate) fn new(iter: I) -> Self {
            Self { next: None, iter }
        }
        pub(crate) fn next(&mut self) -> (Option<I::Item>, Unpeeker<'_, I>) {
            (
                self.next.take().or_else(|| self.iter.next()),
                Unpeeker(&mut self.next),
            )
        }
    }

    pub(crate) struct Unpeeker<'a, I: Iterator>(&'a mut Option<I::Item>);

    impl<I: Iterator> Unpeeker<'_, I> {
        pub(crate) fn unpeek(self, item: Option<I::Item>) {
            *self.0 = item;
        }
    }
}
use unpeekable::Unpeekable;

fn escape_string_with_character_references(mut text: &[u8], result: &mut Vec<u8>) {
    while let &[first, ref after_first @ ..] = text {
        text = after_first;
        if first == b'&'
            && let Some(s) = decode_character_reference_escape(&mut text).as_deref()
        {
            result.extend_from_slice(s.as_bytes());
            continue;
        }
        if memchr(first, b"\\\"").is_some() {
            result.push(b'\\');
        }
        result.push(first);
    }
}

fn decode_character_reference_escape(text: &mut &[u8]) -> Option<CowStr<'static>> {
    if let Some(rest) = text.strip_prefix(b"#") {
        let (digits, after_semi);
        let codepoint =
            if let Some(rest) = rest.strip_prefix(b"x").or_else(|| rest.strip_prefix(b"X")) {
                (digits, after_semi) = take_until_semi(rest, u8::is_ascii_hexdigit)?;
                let digits = unsafe { str::from_utf8_unchecked(digits) };
                u32::from_str_radix(digits, 16).ok()?
            } else {
                (digits, after_semi) = take_until_semi(rest, u8::is_ascii_digit)?;
                let digits = unsafe { str::from_utf8_unchecked(digits) };
                digits.parse::<u32>().ok()?
            };

        let codepoint = char::try_from(codepoint).ok()?;
        if codepoint == '\r'
            || is_noncharacter(codepoint)
            || codepoint.is_control() && !codepoint.is_ascii_whitespace()
        {
            return None;
        }

        *text = after_semi;

        Some(match codepoint {
            '\\' => "\\\\".into(),
            '\"' => "\\\"".into(),
            c => c.into(),
        })
    } else {
        let (name, rest) = take_until_semi(text, u8::is_ascii_alphanumeric)?;
        let resolved = entities::ENTITIES.get(name)?;
        *text = rest;
        Some(CowStr::Borrowed(resolved))
    }
}

#[test]
fn test_character_reference() {
    let farm = Farm::default();
    fn run<'a>(farm: &'a Farm, s: &'a str) -> Option<(&'a str, &'a str)> {
        let ptr = s.as_ptr();
        let mut bytes = s.as_bytes();
        match decode_character_reference_escape(&mut bytes) {
            Some(escaped) => Some((farm.add(escaped), str::from_utf8(bytes).unwrap())),
            None => {
                assert_eq!(bytes.as_ptr(), ptr);
                None
            }
        }
    }
    let run = |s| run(&farm, s);
    assert_eq!(run(""), None);
    assert_eq!(run(";"), None);
    assert_eq!(run("#38"), None);
    assert_eq!(run("#38;"), Some(("&", "")));
    assert_eq!(run("#38;;"), Some(("&", ";")));
    assert_eq!(run("#x26;"), Some(("&", "")));
    assert_eq!(run("#X26;;"), Some(("&", ";")));
    assert_eq!(run("#x110000;"), None);
    assert_eq!(run("#xfdD0;"), None);
    assert_eq!(run("#x000d;"), None);
    assert_eq!(run("#xa;"), Some(("\n", "")));
    assert_eq!(run("#000000000;"), None);
    assert_eq!(run("am;"), None);
    assert_eq!(run("amp"), None);
    assert_eq!(run("amp;"), Some(("&", "")));
    assert_eq!(run("amp;&"), Some(("&", "&")));
    assert_eq!(run("bscr;"), Some(("𝒷", "")));

    assert_eq!(run("#x22;"), Some(("\\\"", "")));
    assert_eq!(run("#x5c;"), Some(("\\\\", "")));
    assert_eq!(run("quot;"), Some(("\\\"", "")));
    assert_eq!(run("QUOT;"), Some(("\\\"", "")));
    assert_eq!(run("bsol;"), Some(("\\\\", "")));
}

fn is_noncharacter(c: char) -> bool {
    ('\u{FDD0}'..='\u{FDEF}').contains(&c) || u32::from(c) & 0xFFFE == 0xFFFE
}

#[test]
fn test_noncharacter() {
    assert!(!is_noncharacter('\u{FDCF}'));
    assert!(is_noncharacter('\u{FDD0}'));
    assert!(is_noncharacter('\u{FDD8}'));
    assert!(is_noncharacter('\u{FDEF}'));
    assert!(!is_noncharacter('\u{FDF0}'));
    assert!(!is_noncharacter('\u{FFFD}'));
    assert!(is_noncharacter('\u{FFFE}'));
    assert!(is_noncharacter('\u{FFFF}'));
    assert!(is_noncharacter('\u{10FFFF}'));
}

fn take_until_semi(s: &[u8], mut f: impl FnMut(&u8) -> bool) -> Option<(&[u8], &[u8])> {
    let i = s.iter().take_while(|b| f(b)).count();
    if i == 0 {
        return None;
    }
    let (before, after) = s.split_at(i);
    let after = after.strip_prefix(b";")?;
    Some((before, after))
}

mod entities {
    include!(concat!(env!("OUT_DIR"), "/entities.rs"));
}

fn escape_string(text: &[u8], result: &mut Vec<u8>) {
    let mut prev_i = 0;
    for i in memchr2_iter(b'\\', b'\"', text) {
        result.extend_from_slice(&text[prev_i..i]);
        result.push(b'\\');
        prev_i = i;
    }
    result.extend_from_slice(&text[prev_i..]);
}

fn escape_text(text: &[u8], result: &mut Vec<u8>) {
    for &byte in text {
        // For shorthands used in markup mode, see:
        // <https://github.com/typst/typst/blob/f51cb4b03e9712b8eb404f8184a17ee4a63aaab2/crates/typst-syntax/src/ast.rs#L592-L597>
        // rustfmt appears to have a bug that tries to move all the comments to the end of the
        // previous line.
        #[rustfmt::skip]
        let to_escape = concat!(
            // bold, italic, monospace/raw
            "*_`",
            // label start
            "<",
            // reference
            "@",
            // heading
            "=",
            // bullet list, en/em dashes, minus sign, soft hyphen
            "-",
            // numbered list
            "+",
            // ellipses, `1.`-style lists
            ".",
            // term list, comments, bare URLs
            "/",
            // math
            "$",
            // line break, character escapes
            "\\",
            // smart quotes
            "'\"",
            // non-breaking space
            "~",
            // code
            "#",
            // beginning and end of content blocks
            "[]",
        );
        if memchr(byte, to_escape.as_bytes()).is_some() {
            result.push(b'\\');
        }
        result.push(byte);
    }
}

struct LabelTracker<'a> {
    counts: HashMap<Label, u64>,
    prefix: &'a str,
}

impl LabelTracker<'_> {
    fn write_after_heading(&mut self, mut label: Label, result: &mut Vec<u8>) {
        if label.0.is_empty() {
            return;
        }
        // Newline needed before so that the label applies to the heading and not the text inside
        // the heading.
        result.extend_from_slice(b"\n#label(\"");
        escape_string(self.prefix.as_bytes(), result);
        escape_string(label.0.as_bytes(), result);
        match self.counts.get(&label) {
            // Don't use `.entry_ref` + `.insert` because that requires cloning. We can't use
            // `.entry` because that unconditionally drops the label in the `Occupied` case.
            None => _ = self.counts.insert(label, 0),
            Some(&(mut n)) => {
                let original_len = label.0.len();
                loop {
                    n += 1;
                    label.0.push('-');
                    label.0.push_str(itoa::Buffer::new().format(n));
                    let collides = self.counts.contains_key(&label);
                    label.0.truncate(original_len);
                    if !collides {
                        break;
                    }
                }
                *self.counts.get_mut(&label).unwrap() = n;
                result.push(b'-');
                result.extend_from_slice(itoa::Buffer::new().format(n).as_bytes());
            }
        }
        result.extend_from_slice(b"\");");
    }
}

#[derive(Default, PartialEq, Eq, Hash)]
struct Label(String);

impl Label {
    fn push(&mut self, s: &str, mode: HeadingLabels) {
        match mode {
            HeadingLabels::GitHub => {
                for c in s.chars() {
                    if github_slug_disallow::SET.contains_char(c) {
                        continue;
                    }
                    if c == ' ' {
                        self.0.push('-');
                    } else {
                        self.0.extend(c.to_lowercase());
                    }
                }
            }
            HeadingLabels::Jupyter => {
                let mut prev_i = 0;
                for i in memchr_iter(b' ', s.as_bytes()) {
                    self.0.push_str(&s[prev_i..i]);
                    self.0.push('-');
                    prev_i = i + 1;
                }
                self.0.push_str(&s[prev_i..]);
            }
        }
    }
}

mod github_slug_disallow {
    include!(concat!(env!("OUT_DIR"), "/github_slug_disallow.rs"));
}

mod farm;
use farm::Farm;

#[cfg(test)]
mod tests {
    #[test]
    fn heading() {
        assert_eq!(with_h1_level("x\n# H", -1), "x\n\nH");
        assert_eq!(with_h1_level("# H", 0), "#set document(title:[H]);#title();");
        assert_eq!(with_h1_level("## H", 0), "\n= H\n#label(\"h\");");
        assert_eq!(with_h1_level("### H", 0), "\n== H\n#label(\"h\");");
        assert_eq!(with_h1_level("# H", 1), "\n= H\n#label(\"h\");");
        assert_eq!(with_h1_level("## H", 1), "\n== H\n#label(\"h\");");
        assert_eq!(with_h1_level("### H", 1), "\n=== H\n#label(\"h\");");
        assert_eq!(with_h1_level("###### H", 1), "\n====== H\n#label(\"h\");");
        assert_eq!(with_h1_level("# H", 3), "\n=== H\n#label(\"h\");");
        assert_eq!(
            with_h1_level("###### H", 4),
            "\n========= H\n#label(\"h\");"
        );
        assert_eq!(with_h1_level("H\n=", 1), "\n= H\n#label(\"h\");");
        assert_eq!(with_h1_level("H\n-", 1), "\n== H\n#label(\"h\");");
        assert_eq!(with_h1_level("# H\n_", 1), "\n= H\n#label(\"h\");\n\\_");
    }

    #[test]
    fn heading_labels() {
        assert_eq!(render("# α 三"), "\n= α 三\n#label(\"α-三\");");
        assert_eq!(render("# _:-.-"), "\n= \\_:\\-\\.\\-\n#label(\"_--\");");
        assert_eq!(
            render("# a`b`c"),
            "\n= a#raw(block:false,\"b\");c\n#label(\"abc\");"
        );
        assert_eq!(render("# a<s>b</s>c"), "\n= abc\n#label(\"abc\");");
        assert_eq!(
            with_math("# a$b + 1$c"),
            "\n= a#inlinemath(\"b + 1\");c\n#label(\"ab--1c\");"
        );
        assert_eq!(
            render("# a\n# a"),
            "\n= a\n#label(\"a\");\n\n= a\n#label(\"a-1\");"
        );
        assert_eq!(
            render("# a\n# a\n# a"),
            "\n= a\n#label(\"a\");\n\n= a\n#label(\"a-1\");\n\n= a\n#label(\"a-2\");"
        );
        assert_eq!(
            render("# a\n# a-1\n# a"),
            "\n= a\n#label(\"a\");\n\n= a\\-1\n#label(\"a-1\");\n\n= a\n#label(\"a-2\");"
        );
        assert_eq!(
            render("# a-2\n# a\n# a\n# a"),
            concat!(
                "\n= a\\-2\n#label(\"a-2\");\n",
                "\n= a\n#label(\"a\");\n",
                "\n= a\n#label(\"a-1\");\n",
                "\n= a\n#label(\"a-3\");",
            ),
        );
        assert_eq!(
            render("# a-1\n# a\n# a\n# a-1"),
            concat!(
                "\n= a\\-1\n#label(\"a-1\");\n",
                "\n= a\n#label(\"a\");\n",
                "\n= a\n#label(\"a-2\");\n",
                "\n= a\\-1\n#label(\"a-1-1\");",
            ),
        );
        // A couple of examples from:
        // <https://github.com/Flet/github-slugger/blob/3461c4350868329c8530904d170358bca1d31448/test/fixtures.json>
        assert_eq!(
            render("# I ♥ unicode"),
            "\n= I ♥ unicode\n#label(\"i--unicode\");"
        );
        assert_eq!(
            render("# a_ ‿ ⁀ ⁔ ︳ ︴ ﹍ ﹎ ﹏ ＿b"),
            "\n= a\\_ ‿ ⁀ ⁔ ︳ ︴ ﹍ ﹎ ﹏ ＿b\n#label(\"a_-‿-⁀-⁔-︳-︴-﹍-﹎-﹏-＿b\");"
        );
        assert_eq!(
            render("# a- ֊ ־ ᐀ ᠆ ‐ ‑ ‒ – — ― ⸗ ⸚ ⸺ ⸻ ⹀ 〜 〰 ゠ ︱ ︲ ﹘ ﹣ －b"),
            "\n= a\\- ֊ ־ ᐀ ᠆ ‐ ‑ ‒ – — ― ⸗ ⸚ ⸺ ⸻ ⹀ 〜 〰 ゠ ︱ ︲ ﹘ ﹣ －b\n#label(\"a------------------------b\");",
        );
    }

    #[test]
    fn heading_label_mode() {
        assert_eq!(
            with_heading_labels("# AΒ Cd", HeadingLabels::GitHub),
            "\n= AΒ Cd\n#label(\"aβ-cd\");"
        );
        assert_eq!(
            with_heading_labels("# AΒ Cd", HeadingLabels::Jupyter),
            "\n= AΒ Cd\n#label(\"AΒ-Cd\");"
        );
    }

    #[test]
    fn lines() {
        assert_eq!(render("a\nb"), "a b");
        assert_eq!(render("a \nb"), "a b");
        assert_eq!(render("a  \nb"), "a\\ b");
        assert_eq!(render("a\n\nb"), "a\n\nb");
    }

    #[test]
    fn styling() {
        assert_eq!(render("*i* _i_"), "#emph[i]; #emph[i];");
        assert_eq!(render("**b** __b__"), "#strong[b]; #strong[b];");
        assert_eq!(render("~s~"), "#strike[s];");
    }

    #[test]
    fn links_images() {
        assert_eq!(
            render("<https://example.org/\">"),
            "#link(\"https://example.org/\\\"\")[https:\\/\\/example\\.org\\/\\\"];"
        );
        assert_eq!(
            render("[a](https://example.org)"),
            "#link(\"https://example.org\")[a];"
        );
        assert_eq!(
            render("[a][a]\n\n[a]: https://example.org"),
            "#link(\"https://example.org\")[a];"
        );
        assert_eq!(
            render("![alt text](https://example.org/)"),
            "#image(\"https://example.org/\",alt:\"alt text\");",
        );
        assert_eq!(
            with_math("![a![*b*]()`c`<p>  \nd\ne$f$]()\n"),
            "#image(\"\",alt:\"abc<p>\nd ef\");"
        );
        assert_eq!(render("[x](#r)"), "#link(label(\"r\"))[x];");
    }

    #[test]
    fn ref_links() {
        // Normal broken links still work as expected.
        assert_eq!(render("[foo]"), "\\[foo\\]");

        assert_eq!(render("[@foo]"), "#ref(label(\"foo\"));");
        assert_eq!(render("[@foo][]"), "#ref(label(\"foo\"));");
        assert_eq!(
            render("[_sup_][@foo]"),
            "#(s=>ref(label(\"foo\"),supplement:s))[#emph[sup];];"
        );
    }

    #[test]
    fn code() {
        assert_eq!(render("\tlet x = 5;"), "#raw(block:true,\"let x = 5;\")");
        assert_eq!(
            render("```\nlet x = 5;\n```"),
            "#raw(block:true,\"let x = 5;\n\")"
        );
        assert_eq!(
            render("```rust\nlet x = 5;\n```"),
            "#raw(block:true,lang:\"rust\",\"let x = 5;\n\")"
        );
        assert_eq!(
            render("some`inline code`…"),
            "some#raw(block:false,\"inline code\");…"
        );
        assert_eq!(render("```\n_\n```\n_"), "#raw(block:true,\"_\n\")\n\\_");
    }

    #[test]
    fn horiz() {
        assert_eq!(render("---\n"), "#rule()");
        assert_eq!(render("---\n_"), "#rule()\n\\_");
    }

    #[test]
    fn lists() {
        assert_eq!(render("- a\n- b\n- c"), "#list([a],[b],[c],)");
        assert_eq!(render("+ a\n+ b\n+ c"), "#list([a],[b],[c],)");
        assert_eq!(render("1. a\n1. b\n1. c"), "#enum(start:1,[a],[b],[c],)");
        assert_eq!(render("5. a\n1. b\n1. c"), "#enum(start:5,[a],[b],[c],)");
        assert_eq!(render("- a\n\n\tb\n\nc"), "#list([a\n\nb\n\n],)\nc");
    }

    #[test]
    fn footnote() {
        assert_eq!(
            render("a [^1].\n\n[^1]: b\n\nc"),
            "a #footnote[b\n\n]#label(\"1\");\\.\n\nc",
        );
        assert_eq!(
            render("[^a]: [^b]\n[^b]: [^a]\n[^unused]: owo\n[^c]: [^a]\n\n[^c]"),
            "#footnote[#footnote[#footnote[#ref(label(\"a\"));\n\n]#label(\"b\");\n\n]#label(\"a\");\n\n]#label(\"c\");",
        );
        assert_eq!(
            render("[^a]: xyz\n\nx[^a][^b][^a]\n\n[^b]: y"),
            "x#footnote[xyz\n\n]#label(\"a\");#footnote[y\n\n]#label(\"b\");#ref(label(\"a\"));",
        );
        assert_eq!(
            render("[^a]:>[^b]:\n  >[^a][^b]"),
            "#quote(block:true)[#footnote[#quote(block:true)[]\n\n]#label(\"a\");#footnote[]#label(\"b\");\n\n]"
        );
        assert_eq!(render("(\n[^x]:\n[^x]"), "(\n\n#footnote[]#label(\"x\");");
    }

    #[test]
    fn escaping() {
        assert_eq!(render("\\*a\\*"), "\\*a\\*");
        assert_eq!(render("\\_a\\_"), "\\_a\\_");
        assert_eq!(render("\\`a\\`"), "\\`a\\`");
    }

    #[test]
    fn smart_punct() {
        assert_eq!(with_smart_punct("\"x\""), "“x”");
        assert_eq!(with_smart_punct("\\\"x\\\""), "\\\"x\\\"");
        assert_eq!(render("\"x\""), "\\\"x\\\"");
        assert_eq!(render("--;---"), "\\-\\-;\\-\\-\\-");
        assert_eq!(with_smart_punct("--;---"), "–;—");
        assert_eq!(with_smart_punct("\\--;\\-\\--"), "\\-\\-;\\-\\-\\-");
    }

    #[test]
    fn blockquote() {
        assert_eq!(render("> *q*"), "#quote(block:true)[#emph[q];\n\n]");
        assert_eq!(
            render("> Quoted\n> > Nested"),
            "#quote(block:true)[Quoted\n\n#quote(block:true)[Nested\n\n]\n\n]"
        );
    }

    #[test]
    fn math() {
        assert_eq!(with_math("$x$"), "#inlinemath(\"x\");");
        assert_eq!(
            with_math("$\\alpha + \\beta$"),
            "#inlinemath(\"\\\\alpha + \\\\beta\");"
        );
        assert_eq!(with_math("$$x$$"), "#displaymath(\"x\");");
        assert_eq!(with_math("a$x$b"), "a#inlinemath(\"x\");b");
        assert_eq!(render("a$x$b"), "a\\$x\\$b");
        assert_eq!(render("a$$x$$b"), "a\\$\\$x\\$\\$b");
        assert_eq!(with_math("$$\nx\n$$"), "#displaymath(\"\nx\n\");");
    }

    #[test]
    fn exclude() {
        assert_eq!(
            render("<!--typst-begin-exclude-->\na\n<!--typst-end-exclude-->"),
            "",
        );
        assert_eq!(
            render("a<!--typst-begin-exclude-->b\nc\nd<!--typst-end-exclude-->e"),
            "ae",
        );
        assert_eq!(render("<!--typst-begin-exclude-->b\n\nc"), "");
        assert_eq!(
            render("<!--typst-begin-exclude-->a<!--typst-end-exclude-->b"),
            "b",
        );
        assert_eq!(
            render("<!--typst-begin-exclude-->\nb<!--typst-end-exclude-->c"),
            "c",
        );
        assert_eq!(render("a<!--typst-begin-exclude-->b\n\nc"), "a");
    }

    #[test]
    fn html_basic() {
        assert_eq!(render("<p>\""), "\\\"");
        assert_eq!(render("<p><"), "\\<");
        assert_eq!(render("<p>&amp;"), "#(\"&\");");
        assert_eq!(render("<p>&amp"), "&amp");
        assert_eq!(render("<p>\\<\\"), "\\\\\\<\\\\");
        assert_eq!(render("<!u&#36;"), "\\<!u#(\"$\");");
    }

    #[test]
    fn html_open_tags() {
        // Tag names, and non-existent and existent tags
        assert_eq!(with_html("<p"), "\\<p");
        assert_eq!(with_html("<p "), "\\<p ");
        assert_eq!(with_html("<p>"), "");
        assert_eq!(with_html("<A-2>"), "");
        assert_eq!(with_html("<p><2>"), "\\<2>");
        assert_eq!(with_html("<p attr=val>"), "");
        assert_eq!(with_html("<b>"), "#(html.b)((:))[]");
        assert_eq!(with_html("<B>"), "#(html.b)((:))[]");
        assert_eq!(with_html("<b/>"), "#(html.b)((:))[]");
        assert_eq!(with_html("<b />"), "#(html.b)((:))[]");

        // Attribute names
        assert_eq!(with_html("<p _ = _ "), "\\<p \\_ \\= \\_ ");
        assert_eq!(with_html("<b _ = _  >"), "#(html.b)((\"_\":\"_\",))[]");
        assert_eq!(with_html("<b :9_.:->"), "#(html.b)((\":9_.:-\":\"\",))[]");
        assert_eq!(
            with_html("<b :9_.:-=_>"),
            "#(html.b)((\":9_.:-\":\"_\",))[]"
        );
        assert_eq!(with_html("<p :`>"), "\\<p :\\`>");
        assert_eq!(with_html("<b u=a u=b>"), "#(html.b)((\"u\":\"a\",))[]");

        // Attribute values
        assert_eq!(with_html("<b _=\\>"), "#(html.b)((\"_\":\"\\\\\",))[]");
        assert_eq!(with_html("<p _==>"), "\\<p \\_\\=\\=>");
        assert_eq!(with_html("<p _=<>"), "\\<p \\_\\=\\<>");
        assert_eq!(with_html("<p _=_'>"), "\\<p \\_\\=\\_\\'>");
        assert_eq!(with_html("<p _=_\">"), "\\<p \\_\\=\\_\\\">");
        assert_eq!(
            with_html("<b _=\"=<>\\\">"),
            "#(html.b)((\"_\":\"=<>\\\\\",))[]"
        );
        assert_eq!(
            with_html("<b _='=<>\\'>"),
            "#(html.b)((\"_\":\"=<>\\\\\",))[]"
        );
        assert_eq!(
            with_html("<b _=_\t__>"),
            "#(html.b)((\"_\":\"_\",\"__\":\"\",))[]"
        );

        // Void tags, raw text tags
        assert_eq!(with_html("<br>"), "#(html.br)((:));");
        assert_eq!(
            with_html("<script>&amp;\""),
            "#(html.script)((:),\"&amp;\\\"\");"
        );
        assert_eq!(
            with_html("x<script>&amp;\""),
            "x#(html.script)((:),\"&\\\"\n\n\");"
        );
        assert_eq!(with_html("<title>&amp;\""), "#(html.title)((:),\"&\\\"\");");
        assert_eq!(
            with_html("x<title>&amp;amp;\""),
            "x#(html.title)((:),\"&amp;\\\"\n\n\");"
        );
        assert_eq!(
            with_html("<script>x</script</script>y"),
            "#(html.script)((:),\"x</script\");y"
        );
        assert_eq!(
            with_html("<script>\n\nx</script>y"),
            "#(html.script)((:),\"\n\nx\");y"
        );
        assert_eq!(
            with_html("<title>\n\n- x</title>\n</title>y"),
            "#(html.title)((:),\"\nx</title>\");y"
        );
    }

    #[test]
    fn html_close_tags() {
        assert_eq!(with_html("<p></0>"), "\\<\\/0>");
        assert_eq!(with_html("</A-2>"), "");
        assert_eq!(with_html("</p >"), "");

        // Closing and autoclosing
        assert_eq!(with_html("<b>a</b>b"), "#(html.b)((:))[a];b");
        assert_eq!(with_html("<b>a</B>b"), "#(html.b)((:))[a];b");
        assert_eq!(
            with_html("<b><em>a</b>b"),
            "#(html.b)((:))[#(html.em)((:))[a];];b"
        );
        assert_eq!(
            with_html("<b><em>a</p>b"),
            "#(html.b)((:))[#(html.em)((:))[ab]]"
        );
        assert_eq!(
            with_html("<em><b>a</b>b</em>"),
            "#(html.em)((:))[#(html.b)((:))[a];b];"
        );
    }

    #[test]
    fn html_comments() {
        assert_eq!(render("<!-- -- -- -- -->x"), "x");
        assert_eq!(render("x<!-- -- -- -- -->y"), "xy");
        assert_eq!(render("<!--\n\n\n\n\n\t-->x"), "x");
        assert_eq!(render("-\n\t<!--\n\t-->"), "#list([  ],)");
    }

    #[test]
    fn autoclosing() {
        assert_eq!(
            with_html("x<b>*<b>y*z</b>w"),
            "x#(html.b)((:))[#emph[#(html.b)((:))[y]];z];w"
        );
        assert_eq!(with_html("- <b>x\n- y"), "#list([#(html.b)((:))[x]],[y],)");
        assert_eq!(with_html("a<b>b*c</b>d*e"), "a#(html.b)((:))[b#emph[cd];e]");
    }

    #[test]
    fn raw_typst() {
        assert_eq!(render("a<!--raw-typst #(1+1)-->b"), "ab");
        assert_eq!(with_raw_typst("a<!--raw-typst#(1+1)-->b"), "a#(1+1)b");
        assert_eq!(
            with_raw_typst("<!--raw-typst\n\n#(1+1)\n\n-->\nb"),
            "\n\n#(1+1)\n\n\nb"
        );
        assert_eq!(render("<!--raw-typst\n\n#(1+1)\n\n-->\nb"), "\nb");
        assert_eq!(render("<!--raw-typst #(1+1)-->a"), "a");
    }

    #[test]
    fn table() {
        let example_md = concat!(
            "| Column 1      | Column 2      |\n",
            "| ------------- | ------------- |\n",
            "| Cell 1, Row 1 | Cell 2, Row 1 |\n",
            "| Cell 1, Row 2 | Cell 2, Row 2 |",
        );
        let example_typst = concat!(
            "#table(align:(auto,auto,),columns:2,",
            "table.header([Column 1],[Column 2],),",
            "[Cell 1, Row 1],[Cell 2, Row 1],",
            "[Cell 1, Row 2],[Cell 2, Row 2],);",
        );
        assert_eq!(render(example_md), example_typst);

        let missing_cell_md = concat!(
            "| a | b | c |\n",
            "| - | - | - |\n",
            "| d | e |\n",
            "| f | g | h | i |",
        );
        let missing_cell_typst = concat!(
            "#table(align:(auto,auto,auto,),columns:3,",
            "table.header([a],[b],[c],),",
            "[d],[e],[],",
            "[f],[g],[h],);",
        );
        assert_eq!(render(missing_cell_md), missing_cell_typst);
    }

    #[test]
    fn prefix() {
        assert_eq!(with_prefix("[@foo]"), "#ref(label(\"u-foo\"));");
        assert_eq!(
            with_prefix("[x][@foo]"),
            "#(s=>ref(label(\"u-foo\"),supplement:s))[x];"
        );
        assert_eq!(with_prefix("[](#foo)"), "#link(label(\"u-foo\"))[];");
        assert_eq!(with_prefix("# h"), "\n= h\n#label(\"l-h\");");
        assert_eq!(
            with_prefix("[^f][^f]\n[^f]: hi"),
            "#footnote[hi\n\n]#label(\"l-f\");#ref(label(\"l-f\"));"
        );
    }

    #[test]
    fn readme() {
        let readme = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../README.md"));
        with_raw_typst(readme);
    }

    #[track_caller]
    fn with_h1_level(s: &str, h1_level: i8) -> String {
        let mut options = default_options();
        options.h1_level = h1_level;
        render_with(s, options)
    }
    #[track_caller]
    fn with_smart_punct(s: &str) -> String {
        let mut options = default_options();
        options.flags = Flags::SMART_PUNCTUATION;
        render_with(s, options)
    }
    #[track_caller]
    fn with_math(s: &str) -> String {
        let mut options = default_options();
        options.flags = Flags::MATH;
        render_with(s, options)
    }
    #[track_caller]
    fn with_raw_typst(s: &str) -> String {
        let mut options = default_options();
        options.flags = Flags::RAW_TYPST;
        render_with(s, options)
    }
    #[track_caller]
    fn with_html(s: &str) -> String {
        let html_tags = HashMap::from_iter(
            [
                ("b", HtmlTagKind::Normal),
                ("em", HtmlTagKind::Normal),
                ("br", HtmlTagKind::Void),
                ("script", HtmlTagKind::RawText),
                ("title", HtmlTagKind::EscapableRawText),
            ]
            .into_iter()
            .map(|(k, v)| (CaseInsensitive(CaseInsensitive(k.as_bytes())), v)),
        );
        let mut options = default_options();
        options.html_tags = &html_tags;
        render_with(s, options)
    }
    #[track_caller]
    fn with_prefix(s: &str) -> String {
        let mut options = default_options();
        options.label_prefix = "l-";
        options.label_use_prefix = "u-";
        render_with(s, options)
    }
    #[track_caller]
    fn with_heading_labels(s: &str, case: HeadingLabels) -> String {
        let mut options = default_options();
        options.heading_labels = case;
        render_with(s, options)
    }
    #[track_caller]
    fn render(s: &str) -> String {
        render_with(s, default_options())
    }
    fn default_options() -> Options<'static, HtmlTagMap<'static, foldhash::fast::FixedState>> {
        let html_tags: &'static HtmlTagMap<'static, _> =
            const { &HashMap::with_hasher(foldhash::fast::FixedState::with_seed(0)) };
        Options {
            html_tags,
            label_prefix: "",
            label_use_prefix: "",
            heading_labels: HeadingLabels::GitHub,
            flags: Flags::empty(),
            h1_level: 1,
        }
    }
    #[track_caller]
    fn render_with(s: &str, options: Options<'_, impl HtmlTags>) -> String {
        let s = String::from_utf8(super::run(s, options).unwrap()).unwrap();
        if let Some(e) = typst_syntax::parse(&s).errors().into_iter().next() {
            panic!("{}\nsource code: ```\n{s}\n```", e.message);
        }
        s
    }

    #[test]
    fn string_escaping() {
        assert_eq!(escape_string(""), "");
        assert_eq!(escape_string("abc#@"), "abc#@");
        assert_eq!(escape_string("\""), "\\\"");
        assert_eq!(escape_string("a\\b\"c\""), "a\\\\b\\\"c\\\"");
    }

    #[test]
    fn text_escaping() {
        assert_eq!(escape_text("http://"), "http:\\/\\/");
        assert_eq!(escape_text("*"), "\\*");
        assert_eq!(escape_text("`"), "\\`");
        assert_eq!(escape_text("⟪<>⟫"), "⟪\\<>⟫");
        assert_eq!(escape_text("\"\'--"), "\\\"\\\'\\-\\-");
        assert_eq!(escape_text("..."), "\\.\\.\\.");
    }

    fn escape_string(s: &str) -> String {
        let mut result = Vec::new();
        super::escape_string(s.as_bytes(), &mut result);
        String::from_utf8(result).unwrap()
    }

    fn escape_text(s: &str) -> String {
        let mut result = Vec::new();
        super::escape_text(s.as_bytes(), &mut result);
        String::from_utf8(result).unwrap()
    }

    use crate::CaseInsensitive;
    use crate::Flags;
    use crate::HeadingLabels;
    use crate::HtmlTagKind;
    use crate::HtmlTagMap;
    use crate::HtmlTags;
    use crate::Options;
    use alloc::string::String;
    use alloc::vec::Vec;
    use hashbrown::HashMap;
}

use alloc::format;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
use bitflags::bitflags;
use core::borrow::Borrow;
use core::hash::BuildHasher;
use core::hash::Hash;
use core::hash::Hasher;
use core::mem;
use core::str;
use hashbrown::DefaultHashBuilder;
use hashbrown::HashMap;
use hashbrown::HashSet;
use memchr::memchr;
use memchr::memchr_iter;
use memchr::memchr2;
use memchr::memchr2_iter;
use memchr::memmem;
use pulldown_cmark::Alignment;
use pulldown_cmark::CowStr;
