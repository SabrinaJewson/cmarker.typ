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

    // TODO: Enable tables
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

use bitflags::bitflags;
use memchr::memchr;
use memchr::memmem;
use pulldown_cmark::CowStr;
use std::str;
use wasm_minimal_protocol::wasm_func;
