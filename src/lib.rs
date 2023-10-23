#![allow(clippy::items_after_test_module)]

wasm_minimal_protocol::initiate_protocol!();

bitflags! {
    #[repr(transparent)]
    struct Options: u8 {
        const SMART_PUNCTUATION = 0b0000_0001;
        const BLOCKQUOTE = 0b0000_0010;
        const RAW_TYPST = 0b0000_0100;
    }
}

#[wasm_func]
fn render(markdown: &[u8], options: &[u8]) -> Result<Vec<u8>, String> {
    let &[options, h1_level] = options else { panic!() };
    let options = Options::from_bits(options).unwrap();
    let markdown = str::from_utf8(markdown).unwrap();
    inner(markdown, options, h1_level)
}

fn inner(markdown: &str, options: Options, h1_level: u8) -> Result<Vec<u8>, String> {
    // TODO: Enable tables and footnotes
    let mut markdown_options = pulldown_cmark::Options::ENABLE_STRIKETHROUGH;
    if options.contains(Options::SMART_PUNCTUATION) {
        markdown_options |= pulldown_cmark::Options::ENABLE_SMART_PUNCTUATION;
    }

    let mut result = Vec::new();

    let mut parser = pulldown_cmark::Parser::new_ext(markdown, markdown_options);
    let mut next_event = None;
    while let Some(event) = next_event.take().or_else(|| parser.next()) {
        use pulldown_cmark::CodeBlockKind;
        use pulldown_cmark::Event::*;
        use pulldown_cmark::Tag::*;
        match event {
            Start(Paragraph) => {}
            End(Paragraph) => result.extend_from_slice(b"\n\n"),

            Start(Heading(level, _, _)) => {
                result.push(b'\n');
                let equal_signs = level as usize - 1 + usize::from(h1_level);
                result.resize(result.len() + equal_signs, b'=');
                result.push(b' ');
            }
            End(Heading(_, _, _)) => result.extend_from_slice(b"\n"),

            Start(BlockQuote) => {
                if options.contains(Options::BLOCKQUOTE) {
                    result.extend_from_slice(b"#blockquote[");
                }
            }
            End(BlockQuote) => {
                if options.contains(Options::BLOCKQUOTE) {
                    result.extend_from_slice(b"]\n\n");
                }
            }

            Start(Strikethrough) => {
                result.extend_from_slice(b"#strike[");
            }
            End(Strikethrough) => {
                result.extend_from_slice(b"]");
            }

            Start(CodeBlock(kind)) => {
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
                        End(CodeBlock(_)) => break,
                        other => return Err(format!("unexpected {other:?} in code block")),
                    }
                }
                result.extend_from_slice(b"\")\n");
            }
            End(CodeBlock(_)) => unreachable!(),

            Start(List(first)) => {
                if let Some(first) = first {
                    result.extend_from_slice(b"#enum(start:");
                    result.extend_from_slice(itoa::Buffer::new().format(first).as_bytes());
                    result.extend_from_slice(b",");
                } else {
                    result.extend_from_slice(b"#list(");
                }
            }
            End(List(_)) => result.extend_from_slice(b")\n"),
            Start(Item) => result.push(b'['),
            End(Item) => result.extend_from_slice(b"],"),

            // TODO: Tables
            Start(Table(_)) => todo!(),
            End(Table(_)) => todo!(),
            Start(TableHead | TableRow | TableCell) => todo!(),
            End(TableHead | TableRow | TableCell) => todo!(),

            Start(Emphasis) | End(Emphasis) => result.push(b'_'),
            Start(Strong) | End(Strong) => result.push(b'*'),

            Start(Link(_, destination, _)) => {
                result.extend_from_slice(b"#link(\"");
                escape_string(destination.as_bytes(), &mut result);
                result.extend_from_slice(b"\")[");
            }
            End(Link(_, _, _)) => result.push(b']'),

            Start(Image(_, path, _)) => {
                result.extend_from_slice(b"#image(\"");
                escape_string(path.as_bytes(), &mut result);
                result.extend_from_slice(b"\",alt:\"");
                loop {
                    match parser.next().unwrap() {
                        Text(text) => escape_string(text.as_bytes(), &mut result),
                        End(Image(_, _, _)) => break,
                        other => return Err(format!("unexpected {other:?} in image alt text")),
                    }
                }
                result.extend_from_slice(b"\")");
            }
            End(Image(_, _, _)) => unreachable!(),

            Text(text) => escape_text(text.as_bytes(), &mut result),

            Code(code) => {
                result.extend_from_slice(b"#raw(block:false,\"");
                escape_string(code.as_bytes(), &mut result);
                result.extend_from_slice(b"\")");
            }

            // We can’t support HTML, so just obliterate it
            Html(s) => {
                let raw_typst_start = b"<!--raw-typst";
                if memmem::find(s.as_bytes(), b"<!--typst-begin-exclude-->").is_some() {
                    let mut layers = 0;
                    for event in &mut parser {
                        match event {
                            Html(s) => {
                                if memmem::find(s.as_bytes(), b"<!--typst-end-exclude-->").is_some()
                                {
                                    break;
                                }
                            }
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

            SoftBreak => result.push(b' '),
            HardBreak => result.extend_from_slice(b"\\ "),

            Rule => result.extend_from_slice(b"#line(length:100%)\n"),

            // Not possible, since we currently disable these features
            Start(FootnoteDefinition(_)) | End(FootnoteDefinition(_)) => unreachable!(),
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
        assert_eq!(render_("a \nb"), "a  b\n\n");
        assert_eq!(render_("a  \nb"), "a\\ b\n\n");
        assert_eq!(render_("a\n\nb"), "a\n\nb\n\n");
    }

    #[test]
    fn styling() {
        assert_eq!(render_("*i* _i_"), "_i_ _i_\n\n");
        assert_eq!(render_("**b** __b__"), "*b* *b*\n\n");
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
        assert_eq!(with_blockquote("> *q*"), "#blockquote[_q_\n\n]\n\n");
        assert_eq!(render_("> *Quoted*"), "_Quoted_\n\n");
        assert_eq!(
            with_blockquote("> Quoted\n> > Nested"),
            "#blockquote[Quoted\n\n#blockquote[Nested\n\n]\n\n]\n\n"
        );
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

    fn with_h1_level(s: &str, h1_level: u8) -> String {
        render(s, Options::empty(), h1_level)
    }
    fn with_smart_punct(s: &str) -> String {
        render(s, Options::SMART_PUNCTUATION, 1)
    }
    fn with_blockquote(s: &str) -> String {
        render(s, Options::BLOCKQUOTE, 1)
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
use pulldown_cmark::CowStr;
use std::str;
use wasm_minimal_protocol::wasm_func;
