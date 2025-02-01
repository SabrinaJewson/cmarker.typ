#![allow(clippy::items_after_test_module)]

wasm_minimal_protocol::initiate_protocol!();

bitflags! {
    #[repr(transparent)]
    struct Options: u8 {
        const SMART_PUNCTUATION = 0b0000_0001;
        const BLOCKQUOTE = 0b0000_0010;
        const RAW_TYPST = 0b0000_0100;
        const MATH = 0b0000_1000;
    }
}

#[wasm_func]
fn render(markdown: &[u8], options: &[u8]) -> Result<Vec<u8>, String> {
    let &[options, h1_level] = options else {
        panic!()
    };
    let options = Options::from_bits(options).unwrap();
    let markdown = str::from_utf8(markdown).unwrap();
    inner(markdown, options, h1_level)
}

fn inner(markdown: &str, options: Options, h1_level: u8) -> Result<Vec<u8>, String> {
    // TODO: Enable footnotes
    let mut markdown_options =
        pulldown_cmark::Options::ENABLE_STRIKETHROUGH | pulldown_cmark::Options::ENABLE_TABLES;
    if options.contains(Options::SMART_PUNCTUATION) {
        markdown_options |= pulldown_cmark::Options::ENABLE_SMART_PUNCTUATION;
    }
    if options.contains(Options::MATH) {
        markdown_options |= pulldown_cmark::Options::ENABLE_MATH;
    }

    let mut result = Vec::new();

    let mut parser = pulldown_cmark::Parser::new_ext(markdown, markdown_options);
    let mut next_event = None;
    while let Some(event) = next_event.take().or_else(|| parser.next()) {
        use pulldown_cmark::CodeBlockKind;
        use pulldown_cmark::Event::*;
        use pulldown_cmark::{Tag, TagEnd};
        match event {
            Start(Tag::Paragraph) => {}
            End(TagEnd::Paragraph) => result.extend_from_slice(b"\n\n"),

            Start(Tag::Heading {
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
            End(TagEnd::Heading(_)) => result.extend_from_slice(b"\n"),

            Start(Tag::BlockQuote(_)) => {
                if options.contains(Options::BLOCKQUOTE) {
                    result.extend_from_slice(b"#blockquote[");
                }
            }
            End(TagEnd::BlockQuote(_)) => {
                if options.contains(Options::BLOCKQUOTE) {
                    result.extend_from_slice(b"]\n\n");
                }
            }

            Start(Tag::Strikethrough) => {
                result.extend_from_slice(b"#strike[");
            }
            End(TagEnd::Strikethrough) => {
                result.extend_from_slice(b"]");
            }

            Start(Tag::CodeBlock(kind)) => {
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
                    match parser.next().unwrap() {
                        Text(text) => escape_string(text.as_bytes(), &mut result),
                        End(TagEnd::CodeBlock) => break,
                        other => return Err(format!("unexpected {other:?} in code block")),
                    }
                }
                result.extend_from_slice(b"\")\n");
            }
            End(TagEnd::CodeBlock) => unreachable!(),

            Start(Tag::List(first)) => {
                if let Some(first) = first {
                    result.extend_from_slice(b"#enum(start:");
                    result.extend_from_slice(itoa::Buffer::new().format(first).as_bytes());
                    result.extend_from_slice(b",");
                } else {
                    result.extend_from_slice(b"#list(");
                }
            }
            End(TagEnd::List(_)) => result.extend_from_slice(b")\n"),
            Start(Tag::Item) => result.push(b'['),
            End(TagEnd::Item) => result.extend_from_slice(b"],"),

            Start(Tag::Table(alignments)) => {
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
            End(TagEnd::Table) => result.extend_from_slice(b")"),
            Start(Tag::TableHead) => result.extend_from_slice(b"table.header("),
            End(TagEnd::TableHead) => result.extend_from_slice(b"),"),

            Start(Tag::TableRow) => {}
            End(TagEnd::TableRow) => {}

            Start(Tag::TableCell) => result.extend_from_slice(b"["),
            End(TagEnd::TableCell) => result.extend_from_slice(b"],"),

            Start(Tag::Emphasis) => result.extend_from_slice(b"#emph["),
            End(TagEnd::Emphasis) => result.push(b']'),

            Start(Tag::Strong) => result.extend_from_slice(b"#strong["),
            End(TagEnd::Strong) => result.push(b']'),

            Start(Tag::Link {
                link_type: _,
                dest_url,
                title: _,
                id: _,
            }) => {
                result.extend_from_slice(b"#link(\"");
                escape_string(dest_url.as_bytes(), &mut result);
                result.extend_from_slice(b"\")[");
            }
            End(TagEnd::Link) => result.push(b']'),

            Start(Tag::Image {
                link_type: _,
                dest_url,
                title: _,
                id: _,
            }) => {
                result.extend_from_slice(b"#image(\"");
                escape_string(dest_url.as_bytes(), &mut result);
                result.extend_from_slice(b"\",alt:\"");
                loop {
                    match parser.next().unwrap() {
                        Text(text) => escape_string(text.as_bytes(), &mut result),
                        End(TagEnd::Image) => break,
                        other => return Err(format!("unexpected {other:?} in image alt text")),
                    }
                }
                result.extend_from_slice(b"\")");
            }
            End(TagEnd::Image) => unreachable!(),

            Start(
                Tag::FootnoteDefinition(_)
                | Tag::DefinitionList
                | Tag::DefinitionListDefinition
                | Tag::DefinitionListTitle
                | Tag::MetadataBlock(_),
            ) => todo!(),

            End(
                TagEnd::FootnoteDefinition
                | TagEnd::DefinitionList
                | TagEnd::DefinitionListDefinition
                | TagEnd::DefinitionListTitle
                | TagEnd::MetadataBlock(_),
            ) => todo!(),

            Start(Tag::HtmlBlock) => {}
            End(TagEnd::HtmlBlock) => {}

            Text(text) => escape_text(text.as_bytes(), &mut result),

            Code(code) => {
                result.extend_from_slice(b"#raw(block:false,\"");
                escape_string(code.as_bytes(), &mut result);
                result.extend_from_slice(b"\")");
            }

            // We can’t support HTML, so just obliterate it
            Html(s) | InlineHtml(s) => {
                let raw_typst_start = b"<!--raw-typst";
                if memmem::find(s.as_bytes(), b"<!--typst-begin-exclude-->").is_some() {
                    let mut layers = 0;
                    for event in &mut parser {
                        match event {
                            Html(s) | InlineHtml(s) => {
                                if memmem::find(s.as_bytes(), b"<!--typst-end-exclude-->").is_some()
                                {
                                    break;
                                }
                            }
                            Start(Tag::HtmlBlock) => {}
                            End(TagEnd::HtmlBlock) => {}
                            Start(_) => layers += 1,
                            End(_) if layers != 0 => layers -= 1,
                            End(_) => {
                                next_event = Some(event);
                                break;
                            }
                            _ => {}
                        }
                    }
                } else if !options.contains(Options::RAW_TYPST) {
                    // Don’t allow injecting raw Typst code if the user disables it
                } else if let Some(i) = memmem::find(s.as_bytes(), raw_typst_start) {
                    let mut event = CowStr::Borrowed(&s[i + raw_typst_start.len()..]);
                    loop {
                        if let Some(i) = memmem::find(event.as_bytes(), b"-->") {
                            result.extend_from_slice(&event.as_bytes()[..i]);
                            break;
                        }
                        result.extend_from_slice(event.as_bytes());
                        event = match parser.next() {
                            Some(Html(s) | Text(s)) => s,
                            Some(other) => {
                                return Err(format!("unexpected {other:?} in raw typst"))
                            }
                            None => break,
                        };
                    }
                }
            }

            InlineMath(s) => {
                // We use #inlinemath(`…`) for inline math
                result.extend_from_slice(b"#inlinemath(\"");
                escape_string(s.as_bytes(), &mut result);
                result.extend_from_slice(b"\")");
            }

            DisplayMath(s) => {
                // We use #displaymath(`…`) for display math
                result.extend_from_slice(b"#displaymath(\"");
                escape_string(s.as_bytes(), &mut result);
                result.extend_from_slice(b"\")");
            }

            SoftBreak => result.push(b' '),
            HardBreak => result.extend_from_slice(b"\\ "),

            Rule => result.extend_from_slice(b"#line(length:100%)\n"),

            FootnoteReference(_) => unreachable!(),
            TaskListMarker(_) => unreachable!(),
        }
    }

    Ok(result)
}

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
            result.extend_from_slice(b"\")");
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
        assert_eq!(render_("*i* _i_"), "#emph[i] #emph[i]\n\n");
        assert_eq!(render_("**b** __b__"), "#strong[b] #strong[b]\n\n");
        assert_eq!(render_("~s~"), "#strike[s]\n\n");
    }

    #[test]
    fn links_images() {
        assert_eq!(
            render_("<https://example.org/\">"),
            "#link(\"https://example.org/\\\"\")[#(\"https://\")example.org\\/\\\"]\n\n"
        );
        assert_eq!(
            render_("[a](https://example.org)"),
            "#link(\"https://example.org\")[a]\n\n"
        );
        assert_eq!(
            render_("[a][a]\n\n[a]: https://example.org"),
            "#link(\"https://example.org\")[a]\n\n"
        );
        assert_eq!(
            render_("![alt text](https://example.org/)"),
            "#image(\"https://example.org/\",alt:\"alt text\")\n\n",
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
            "some#raw(block:false,\"inline code\")…\n\n"
        );
    }

    #[test]
    fn horiz() {
        assert_eq!(render_("---\n"), "#line(length:100%)\n",);
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
        assert_eq!(with_blockquote("> *q*"), "#blockquote[#emph[q]\n\n]\n\n");
        assert_eq!(render_("> *Quoted*"), "#emph[Quoted]\n\n");
        assert_eq!(
            with_blockquote("> Quoted\n> > Nested"),
            "#blockquote[Quoted\n\n#blockquote[Nested\n\n]\n\n]\n\n"
        );
    }

    #[test]
    fn math() {
        assert_eq!(with_math("$x$"), "#inlinemath(\"x\")\n\n");
        assert_eq!(
            with_math("$\\alpha + \\beta$"),
            "#inlinemath(\"\\\\alpha + \\\\beta\")\n\n"
        );
        assert_eq!(with_math("$$x$$"), "#displaymath(\"x\")\n\n");
        assert_eq!(with_math("a$x$b"), "a#inlinemath(\"x\")b\n\n");
        assert_eq!(render_("a$x$b"), "a\\$x\\$b\n\n");
        assert_eq!(render_("a$$x$$b"), "a\\$\\$x\\$\\$b\n\n");
        assert_eq!(with_math("$$\nx\n$$"), "#displaymath(\"\nx\n\")\n\n");
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
        assert_eq!(render_("a<!--typst-begin-exclude-->b\n\nc"), "a\n\nc\n\n",);
    }

    #[test]
    fn html() {
        assert_eq!(render_("a<!-- … -->b"), "ab\n\n");
        assert_eq!(render_("<p>a</p>"), "");
        assert_eq!(render_("<p>\na</p>"), "");
        assert_eq!(render_("<p>\n\na</p>"), "a\n\n");
    }

    #[test]
    fn raw_typst() {
        assert_eq!(render_("a<!--raw-typst #(1+1)-->b"), "ab\n\n");
        assert_eq!(with_raw_typst("a<!--raw-typst#(1+1)-->b"), "a#(1+1)b\n\n");
        assert_eq!(
            with_raw_typst("<!--raw-typst\n\n#(1+1)\n\n-->\nb"),
            "\n\n#(1+1)\n\nb\n\n"
        );
        assert_eq!(render_("<!--raw-typst\n\n#(1+1)\n\n-->\nb"), "b\n\n");
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
            "[Cell 1, Row 2],[Cell 2, Row 2],)",
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
            "[f],[g],[h],)",
        );
        assert_eq!(render_(missing_cell_md), missing_cell_typst);
    }

    fn with_h1_level(s: &str, h1_level: u8) -> String {
        render(s, Options::empty(), h1_level)
    }
    fn with_smart_punct(s: &str) -> String {
        render(s, Options::SMART_PUNCTUATION, 1)
    }
    fn with_blockquote(s: &str) -> String {
        render(s, Options::BLOCKQUOTE, 1)
    }
    fn with_math(s: &str) -> String {
        render(s, Options::MATH, 1)
    }
    fn with_raw_typst(s: &str) -> String {
        render(s, Options::RAW_TYPST, 1)
    }
    fn render_(s: &str) -> String {
        render(s, Options::empty(), 1)
    }
    fn render(s: &str, options: Options, h1_level: u8) -> String {
        String::from_utf8(super::inner(s, options, h1_level).unwrap()).unwrap()
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
        assert_eq!(escape_text("http://"), "#(\"http://\")");
        assert_eq!(escape_text("foohttps://bar"), "foo#(\"https://\")bar");
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

    use crate::Options;
}

use bitflags::bitflags;
use memchr::memchr;
use memchr::memmem;
use pulldown_cmark::Alignment;
use pulldown_cmark::CowStr;
use std::str;
use wasm_minimal_protocol::wasm_func;
