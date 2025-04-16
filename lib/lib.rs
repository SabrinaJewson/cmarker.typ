#![no_std]
#![allow(clippy::items_after_test_module)]

extern crate alloc;

bitflags! {
    #[repr(transparent)]
    pub struct Options: u8 {
        const SMART_PUNCTUATION = 0b0000_0001;
        const RAW_TYPST = 0b0000_0010;
        const MATH = 0b0000_0100;
    }
}

pub type HtmlTags<'input> = HashMap<CaseInsensitiveKey<&'input [u8]>, HtmlTagKind>;

pub use hashbrown;

#[derive(Clone, Copy)]
pub enum HtmlTagKind {
    Void,
    RawText,
    EscapableRawText,
    Normal,
}

pub fn run(
    markdown: &str,
    html_tags: &HtmlTags<'_>,
    options: Options,
    h1_level: u8,
) -> Result<Vec<u8>, String> {
    let mut markdown_options = pulldown_cmark::Options::empty()
        | pulldown_cmark::Options::ENABLE_STRIKETHROUGH
        | pulldown_cmark::Options::ENABLE_TABLES
        | pulldown_cmark::Options::ENABLE_FOOTNOTES;
    if options.contains(Options::SMART_PUNCTUATION) {
        markdown_options |= pulldown_cmark::Options::ENABLE_SMART_PUNCTUATION;
    }
    if options.contains(Options::MATH) {
        markdown_options |= pulldown_cmark::Options::ENABLE_MATH;
    }

    let mut result = Vec::new();
    let mut open_tags: Vec<Vec<CaseInsensitive<&[u8]>>> = vec![Vec::new()];
    let start_scope = |open_tags: &mut Vec<Vec<CaseInsensitive<&[u8]>>>, result: &mut Vec<u8>| {
        open_tags.push(Vec::new());
        result.extend_from_slice(b"[");
    };
    let end_scope = |open_tags: &mut Vec<Vec<CaseInsensitive<&[u8]>>>, result: &mut Vec<u8>| {
        for _ in open_tags.pop().unwrap() {
            result.extend_from_slice(b"];");
        }
        result.extend_from_slice(b"]");
    };
    let mut farm = Farm::default();

    let mut parser = Unpeekable::new(pulldown_cmark::Parser::new_ext(markdown, markdown_options));
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
                result.push(b'\n');
                let equal_signs = level as usize - 1 + usize::from(h1_level);
                result.resize(result.len() + equal_signs, b'=');
                result.push(b' ');
            }
            E::End(TagEnd::Heading(_)) => result.extend_from_slice(b"\n"),

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
                if let CodeBlockKind::Fenced(lang) = kind {
                    if !lang.is_empty() {
                        result.extend_from_slice(b"lang:\"");
                        escape_string(lang.as_bytes(), &mut result);
                        result.extend_from_slice(b"\",");
                    }
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
                link_type: _,
                dest_url,
                title: _,
                id: _,
            }) => {
                result.extend_from_slice(b"#link(\"");
                escape_string(dest_url.as_bytes(), &mut result);
                result.extend_from_slice(b"\")");
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
                let mut layers = 1_u32;
                while layers != 0 {
                    match parser.next().0.unwrap() {
                        E::Text(s) | E::InlineHtml(s) | E::Code(s) => {
                            escape_string(s.as_bytes(), &mut result)
                        }
                        E::SoftBreak | E::HardBreak => result.push(b' '),
                        E::Start(Tag::Image { .. }) => layers += 1,
                        E::End(TagEnd::Image) => layers -= 1,
                        _ => {}
                    }
                }
                result.extend_from_slice(b"\");");
            }
            E::End(TagEnd::Image) => unreachable!(),

            E::Start(Tag::FootnoteDefinition(label)) => {
                // Because Typst displays footnotes inline while Markdown gives them out-of-line,
                // we use `#show super:s=>none` to make the footnote that would be generated by
                // Typst disappear.
                result.extend_from_slice(b"#[#show super:s=>none;#let l=\"");
                escape_string(label.as_bytes(), &mut result);
                result.extend_from_slice(b"\";#footnote");
                start_scope(&mut open_tags, &mut result);
            }
            E::End(TagEnd::FootnoteDefinition) => {
                end_scope(&mut open_tags, &mut result);
                result.extend_from_slice(b"#label(l)];");
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

            E::Text(text) => escape_text(text.as_bytes(), &mut result),

            E::Code(code) => {
                result.extend_from_slice(b"#raw(block:false,\"");
                escape_string(code.as_bytes(), &mut result);
                result.extend_from_slice(b"\");");
            }

            E::Html(s) | E::InlineHtml(s) => {
                farm.clear();
                let mut cx = HtmlContext {
                    farm: &farm,
                    html: s.as_bytes(),
                    parser: &mut parser,
                    tags: html_tags,
                    open_tags: open_tags.last_mut().unwrap(),
                    result: &mut result,
                    raw_typst: options.contains(Options::RAW_TYPST),
                };
                parse_html(&mut cx);
            }

            E::InlineMath(s) => {
                // We use #inlinemath(`…`) for inline math
                result.extend_from_slice(b"#inlinemath(\"");
                escape_string(s.as_bytes(), &mut result);
                result.extend_from_slice(b"\");");
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

            E::FootnoteReference(label) => {
                result.extend_from_slice(b"#ref(label(\"");
                escape_string(label.as_bytes(), &mut result);
                result.extend_from_slice(b"\"));");
            }

            E::TaskListMarker(_) => unreachable!(),
        }
    }

    for _ in open_tags.pop().unwrap() {
        result.extend_from_slice(b"]");
    }
    assert_eq!(open_tags.len(), 0);

    Ok(result)
}

struct HtmlContext<'a, 'input> {
    farm: &'a Farm,
    html: &'a [u8],
    parser: &'a mut Unpeekable<pulldown_cmark::Parser<'input>>,
    tags: &'a HtmlTags<'input>,
    open_tags: &'a mut Vec<CaseInsensitive<&'input [u8]>>,
    result: &'a mut Vec<u8>,
    raw_typst: bool,
}

fn parse_html(cx: &mut HtmlContext<'_, '_>) {
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

fn html_open_tag(cx: &mut HtmlContext<'_, '_>) -> Option<()> {
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

    let tag = cx.tags.get_key_value(&CaseInsensitive(source_tag_name));
    if let Some((tag_name, _)) = tag {
        result.inner.extend_from_slice(b"#(html.");
        result.inner.extend_from_slice(tag_name.0.0);
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

    if let Some((&tag_name, tag_kind)) = tag {
        if attribute_names.is_empty() {
            cx.result.push(b':');
        }
        cx.result.extend_from_slice(b")");
        match tag_kind {
            HtmlTagKind::Void => cx.result.extend_from_slice(b");"),
            HtmlTagKind::Normal => {
                cx.open_tags.push(tag_name.0);
                cx.result.extend_from_slice(b")[");
            }
            HtmlTagKind::RawText | HtmlTagKind::EscapableRawText => {
                cx.result.extend_from_slice(b",\"");
                let maybe_decode: fn(&[u8], &mut Vec<u8>) = match tag_kind {
                    HtmlTagKind::EscapableRawText => escape_string_with_character_references,
                    HtmlTagKind::RawText => escape_string,
                    _ => unreachable!(),
                };

                'outer: while let Some(text) = cx.refill() {
                    if let Some(text) = text {
                        escape_string(text.as_bytes(), cx.result);
                        continue;
                    }
                    for possible_end in memmem::find_iter(cx.html, b"</") {
                        let (before, after) = cx.html.split_at(possible_end);
                        if let Some(rest) = after[2..]
                            .split_at_checked(tag_name.0.0.len())
                            .filter(|&(s, _)| CaseInsensitive(s) == tag_name.0)
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

fn html_close_tag(cx: &mut HtmlContext<'_, '_>) -> Option<()> {
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

fn html_comment(cx: &mut HtmlContext<'_, '_>) -> Option<()> {
    cx.html = cx.html.strip_prefix(b"!--")?;

    const BEGIN_EXCLUDE: &[u8] = b"typst-begin-exclude-->";
    const END_EXCLUDE: &[u8] = b"<!--typst-end-exclude-->";
    if cx.html.starts_with(BEGIN_EXCLUDE) {
        loop {
            if let Some(end) = memmem::find(cx.html, END_EXCLUDE) {
                cx.html = &cx.html[end + END_EXCLUDE.len()..];
                break;
            }
            cx.html = b"";

            let (Some(event), unpeeker) = cx.parser.next() else {
                break;
            };

            use pulldown_cmark::{Event as E, Tag, TagEnd};
            match event {
                E::Html(s) | E::InlineHtml(s) => cx.html = cx.farm.add(s).as_bytes(),
                E::Start(Tag::HtmlBlock | Tag::Paragraph)
                | E::End(TagEnd::HtmlBlock | TagEnd::Paragraph) => {}
                E::Start(_) => {
                    let mut layers = 1_u32;
                    while layers != 0 {
                        match cx.parser.next().0.unwrap() {
                            E::Start(_) => layers += 1,
                            E::End(_) => layers -= 1,
                            _ => {}
                        }
                    }
                }
                E::End(_) => {
                    unpeeker.unpeek(Some(event));
                    break;
                }
                _ => {}
            }
        }
        return Some(());
    }

    let raw_typst = match (cx.raw_typst, cx.html.strip_prefix(b"raw-typst")) {
        (true, Some(rest)) => {
            cx.html = rest;
            true
        }
        _ => false,
    };

    while let Some(text) = cx.refill() {
        if let Some(text) = text {
            if raw_typst {
                cx.result.extend_from_slice(text.as_bytes());
            }
            continue;
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

impl<'input> HtmlContext<'_, 'input> {
    fn refill(&mut self) -> Option<Option<CowStr<'input>>> {
        if !self.html.is_empty() {
            return Some(None);
        }
        use pulldown_cmark::Event as E;
        match self.parser.next() {
            (Some(E::Text(s)), _) => Some(Some(s)),
            (Some(E::Html(s) | E::InlineHtml(s)), _) => {
                self.html = self.farm.add(s).as_bytes();
                Some(None)
            }
            (other, unpeeker) => {
                unpeeker.unpeek(other);
                None
            }
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
        if first == b'&' {
            if let Some(s) = decode_character_reference_escape(&mut text).as_deref() {
                result.extend_from_slice(s.as_bytes());
                continue;
            }
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

mod entities;

fn escape_string(text: &[u8], result: &mut Vec<u8>) {
    for &byte in text {
        if memchr(byte, b"\\\"").is_some() {
            result.push(b'\\');
        }
        result.push(byte);
    }
}

fn escape_text(text: &[u8], result: &mut Vec<u8>) {
    let mut iter = text.iter().enumerate();
    while let Some((i, &byte)) = iter.next() {
        let prefix = [b"http://" as &[u8], b"https://"]
            .iter()
            .find(|s| text[i..].starts_with(s));
        if let Some(prefix) = prefix {
            result.extend_from_slice(b"#(\"");
            result.extend_from_slice(prefix);
            result.extend_from_slice(b"\");");
            iter.nth(prefix.len() - 2);
            continue;
        }

        let to_escape = b"*_`<>@=-+/$\\'\"~#[]";
        if memchr(byte, to_escape).is_some() {
            result.push(b'\\');
        }
        result.push(byte);
    }
}

mod farm;
use farm::Farm;

#[cfg(test)]
mod tests {
    #[test]
    fn heading() {
        assert_eq!(with_h1_level("# H", 0), "\n H\n");
        assert_eq!(with_h1_level("## H", 0), "\n= H\n");
        assert_eq!(with_h1_level("### H", 0), "\n== H\n");
        assert_eq!(with_h1_level("# H", 1), "\n= H\n");
        assert_eq!(with_h1_level("## H", 1), "\n== H\n");
        assert_eq!(with_h1_level("### H", 1), "\n=== H\n");
        assert_eq!(with_h1_level("###### H", 1), "\n====== H\n");
        assert_eq!(with_h1_level("# H", 3), "\n=== H\n");
        assert_eq!(with_h1_level("###### H", 4), "\n========= H\n");
        assert_eq!(with_h1_level("H\n=", 1), "\n= H\n");
        assert_eq!(with_h1_level("H\n-", 1), "\n== H\n");
    }

    #[test]
    fn lines() {
        assert_eq!(render_("a\nb"), "a b\n\n");
        assert_eq!(render_("a \nb"), "a b\n\n");
        assert_eq!(render_("a  \nb"), "a\\ b\n\n");
        assert_eq!(render_("a\n\nb"), "a\n\nb\n\n");
    }

    #[test]
    fn styling() {
        assert_eq!(render_("*i* _i_"), "#emph[i]; #emph[i];\n\n");
        assert_eq!(render_("**b** __b__"), "#strong[b]; #strong[b];\n\n");
        assert_eq!(render_("~s~"), "#strike[s];\n\n");
    }

    #[test]
    fn links_images() {
        assert_eq!(
            render_("<https://example.org/\">"),
            "#link(\"https://example.org/\\\"\")[#(\"https://\");example.org\\/\\\"];\n\n"
        );
        assert_eq!(
            render_("[a](https://example.org)"),
            "#link(\"https://example.org\")[a];\n\n"
        );
        assert_eq!(
            render_("[a][a]\n\n[a]: https://example.org"),
            "#link(\"https://example.org\")[a];\n\n"
        );
        assert_eq!(
            render_("![alt text](https://example.org/)"),
            "#image(\"https://example.org/\",alt:\"alt text\");\n\n",
        );
        assert_eq!(
            render_("![a![*b*]()`c`<p>  \nd\ne]()\n"),
            "#image(\"\",alt:\"abc<p> d e\");\n\n"
        );
    }

    #[test]
    fn code() {
        assert_eq!(render_("\tlet x = 5;"), "#raw(block:true,\"let x = 5;\")\n");
        assert_eq!(
            render_("```\nlet x = 5;\n```"),
            "#raw(block:true,\"let x = 5;\n\")\n"
        );
        assert_eq!(
            render_("```rust\nlet x = 5;\n```"),
            "#raw(block:true,lang:\"rust\",\"let x = 5;\n\")\n"
        );
        assert_eq!(
            render_("some`inline code`…"),
            "some#raw(block:false,\"inline code\");…\n\n"
        );
    }

    #[test]
    fn horiz() {
        assert_eq!(render_("---\n"), "#rule()\n",);
    }

    #[test]
    fn lists() {
        assert_eq!(render_("- a\n- b\n- c"), "#list([a],[b],[c],)\n");
        assert_eq!(render_("+ a\n+ b\n+ c"), "#list([a],[b],[c],)\n");
        assert_eq!(render_("1. a\n1. b\n1. c"), "#enum(start:1,[a],[b],[c],)\n");
        assert_eq!(render_("5. a\n1. b\n1. c"), "#enum(start:5,[a],[b],[c],)\n");
        assert_eq!(render_("- a\n\n\tb\n\nc"), "#list([a\n\nb\n\n],)\nc\n\n");
    }

    #[test]
    fn footnote() {
        assert_eq!(
            render_("a [^1].\n\n[^1]: b\n\nc"),
            "a #ref(label(\"1\"));.\n\n#[#show super:s=>none;#let l=\"1\";#footnote[b\n\n]#label(l)];c\n\n"
        );
    }

    #[test]
    fn escaping() {
        assert_eq!(render_("\\*a\\*"), "\\*a\\*\n\n");
        assert_eq!(render_("\\_a\\_"), "\\_a\\_\n\n");
        assert_eq!(render_("\\`a\\`"), "\\`a\\`\n\n");
    }

    #[test]
    fn smart_punct() {
        assert_eq!(with_smart_punct("\"x\""), "“x”\n\n");
        assert_eq!(with_smart_punct("\\\"x\\\""), "\\\"x\\\"\n\n");
        assert_eq!(render_("\"x\""), "\\\"x\\\"\n\n");
        assert_eq!(render_("--;---"), "\\-\\-;\\-\\-\\-\n\n");
        assert_eq!(with_smart_punct("--;---"), "–;—\n\n");
        assert_eq!(with_smart_punct("\\--;\\-\\--"), "\\-\\-;\\-\\-\\-\n\n");
    }

    #[test]
    fn blockquote() {
        assert_eq!(render_("> *q*"), "#quote(block:true)[#emph[q];\n\n]\n\n");
        assert_eq!(
            render_("> Quoted\n> > Nested"),
            "#quote(block:true)[Quoted\n\n#quote(block:true)[Nested\n\n]\n\n]\n\n"
        );
    }

    #[test]
    fn math() {
        assert_eq!(with_math("$x$"), "#inlinemath(\"x\");\n\n");
        assert_eq!(
            with_math("$\\alpha + \\beta$"),
            "#inlinemath(\"\\\\alpha + \\\\beta\");\n\n"
        );
        assert_eq!(with_math("$$x$$"), "#displaymath(\"x\");\n\n");
        assert_eq!(with_math("a$x$b"), "a#inlinemath(\"x\");b\n\n");
        assert_eq!(render_("a$x$b"), "a\\$x\\$b\n\n");
        assert_eq!(render_("a$$x$$b"), "a\\$\\$x\\$\\$b\n\n");
        assert_eq!(with_math("$$\nx\n$$"), "#displaymath(\"\nx\n\");\n\n");
    }

    #[test]
    fn exclude() {
        assert_eq!(
            render_("<!--typst-begin-exclude-->\na\n<!--typst-end-exclude-->"),
            "",
        );
        assert_eq!(
            render_("a<!--typst-begin-exclude-->b\nc\nd<!--typst-end-exclude-->e"),
            "ae\n\n",
        );
        assert_eq!(render_("<!--typst-begin-exclude-->b\n\nc"), "",);
        assert_eq!(
            render_("<!--typst-begin-exclude-->a<!--typst-end-exclude-->b"),
            "b",
        );
        assert_eq!(
            render_("<!--typst-begin-exclude-->\nb<!--typst-end-exclude-->c"),
            "c\n\n",
        );
        assert_eq!(render_("a<!--typst-begin-exclude-->b\n\nc"), "a",);
    }

    #[test]
    fn html_basic() {
        assert_eq!(render_("<p>\""), "\\\"");
        assert_eq!(render_("<p><"), "\\<");
        assert_eq!(render_("<p>&amp;"), "#(\"&\");");
        assert_eq!(render_("<p>&amp"), "&amp");
        assert_eq!(render_("<p>\\<\\"), "\\\\\\<\\\\");
        assert_eq!(render_("<!u&#36;"), "\\<!u#(\"$\");");
    }

    #[test]
    fn html_open_tags() {
        // Tag names, and non-existent and existent tags
        assert_eq!(with_html("<p"), "\\<p");
        assert_eq!(with_html("<p "), "\\<p ");
        assert_eq!(with_html("<p>"), "");
        assert_eq!(with_html("<A-2>"), "");
        assert_eq!(with_html("<p><2>"), "\\<2\\>");
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
        assert_eq!(with_html("<p :`>"), "\\<p :\\`\\>");
        assert_eq!(with_html("<b u=a u=b>"), "#(html.b)((\"u\":\"a\",))[]");

        // Attribute values
        assert_eq!(with_html("<b _=\\>"), "#(html.b)((\"_\":\"\\\\\",))[]");
        assert_eq!(with_html("<p _==>"), "\\<p \\_\\=\\=\\>");
        assert_eq!(with_html("<p _=<>"), "\\<p \\_\\=\\<\\>");
        assert_eq!(with_html("<p _=_'>"), "\\<p \\_\\=\\_\\'\\>");
        assert_eq!(with_html("<p _=_\">"), "\\<p \\_\\=\\_\\\"\\>");
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
            "x#(html.script)((:),\"&\\\"\");\n\n"
        );
        assert_eq!(with_html("<title>&amp;\""), "#(html.title)((:),\"&\\\"\");");
        assert_eq!(
            with_html("x<title>&amp;amp;\""),
            "x#(html.title)((:),\"&amp;\\\"\");\n\n"
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
            with_html("<title>\n\nx</title>y"),
            "#(html.title)((:),\"\n\");xy\n\n"
        );
    }

    #[test]
    fn html_close_tags() {
        assert_eq!(with_html("<p></0>"), "\\<\\/0\\>");
        assert_eq!(with_html("</A-2>"), "");
        assert_eq!(with_html("</p >"), "");

        // Closing and autoclosing
        assert_eq!(with_html("<b>a</b>b"), "#(html.b)((:))[a];b\n\n");
        assert_eq!(with_html("<b>a</B>b"), "#(html.b)((:))[a];b\n\n");
        assert_eq!(
            with_html("<b><em>a</b>b"),
            "#(html.b)((:))[#(html.em)((:))[a];];b\n\n"
        );
        assert_eq!(
            with_html("<b><em>a</p>b"),
            "#(html.b)((:))[#(html.em)((:))[ab\n\n]]"
        );
        assert_eq!(
            with_html("<em><b>a</b>b</em>"),
            "#(html.em)((:))[#(html.b)((:))[a];b];\n\n"
        );
    }

    #[test]
    fn html_comments() {
        assert_eq!(render_("<!-- -- -- -- -->x"), "x");
        assert_eq!(render_("x<!-- -- -- -- -->y"), "xy\n\n");
        assert_eq!(render_("<!--\n\n\n\n\n\t-->x"), "x");
        assert_eq!(render_("-\n\t<!--\n\t-->"), "#list([  ],)\n");
    }

    #[test]
    fn autoclosing() {
        assert_eq!(
            with_html("x<b>*<b>y*z</b>w"),
            "x#(html.b)((:))[#emph[#(html.b)((:))[y];];z];w\n\n"
        );
        assert_eq!(
            with_html("- <b>x\n- y"),
            "#list([#(html.b)((:))[x];],[y],)\n",
        );
        assert_eq!(
            with_html("a<b>b*c</b>d*e"),
            "a#(html.b)((:))[b#emph[cd];e\n\n]",
        );
    }

    #[test]
    fn raw_typst() {
        assert_eq!(render_("a<!--raw-typst #(1+1)-->b"), "ab\n\n");
        assert_eq!(with_raw_typst("a<!--raw-typst#(1+1)-->b"), "a#(1+1)b\n\n");
        assert_eq!(
            with_raw_typst("<!--raw-typst\n\n#(1+1)\n\n-->\nb"),
            "\n\n#(1+1)\n\n\nb\n\n"
        );
        assert_eq!(render_("<!--raw-typst\n\n#(1+1)\n\n-->\nb"), "\nb\n\n");
        assert_eq!(render_("<!--raw-typst #(1+1)-->a"), "a");
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
        assert_eq!(render_(example_md), example_typst);

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
        assert_eq!(render_(missing_cell_md), missing_cell_typst);
    }

    #[test]
    fn readme() {
        let readme = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../README.md"));
        with_raw_typst(readme);
    }

    fn with_h1_level(s: &str, h1_level: u8) -> String {
        render(s, &HashMap::new(), Options::empty(), h1_level)
    }
    fn with_smart_punct(s: &str) -> String {
        render(s, &HashMap::new(), Options::SMART_PUNCTUATION, 1)
    }
    fn with_math(s: &str) -> String {
        render(s, &HashMap::new(), Options::MATH, 1)
    }
    fn with_raw_typst(s: &str) -> String {
        render(s, &HashMap::new(), Options::RAW_TYPST, 1)
    }
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
        render(s, &html_tags, Options::empty(), 1)
    }
    fn render_(s: &str) -> String {
        render(s, &HashMap::new(), Options::empty(), 1)
    }
    fn render(s: &str, html_tags: &HtmlTags<'_>, options: Options, h1_level: u8) -> String {
        String::from_utf8(super::run(s, html_tags, options, h1_level).unwrap()).unwrap()
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
        assert_eq!(escape_text("http://"), "#(\"http://\");");
        assert_eq!(escape_text("foohttps://bar"), "foo#(\"https://\");bar");
        assert_eq!(escape_text("*"), "\\*");
        assert_eq!(escape_text("`"), "\\`");
        assert_eq!(escape_text("⟪<>⟫"), "⟪\\<\\>⟫");
        assert_eq!(escape_text("\"\'--"), "\\\"\\\'\\-\\-");
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
    use crate::HtmlTagKind;
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
use core::hash::Hash;
use core::hash::Hasher;
use core::mem;
use core::str;
use hashbrown::HashMap;
use hashbrown::HashSet;
use memchr::memchr;
use memchr::memchr2;
use memchr::memmem;
use pulldown_cmark::Alignment;
use pulldown_cmark::CowStr;
