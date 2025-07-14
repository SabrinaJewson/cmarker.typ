#![no_main]

static HTML_TAGS: LazyLock<HtmlTagMap<'static>> = LazyLock::new(|| {
    [
        ("sub", HtmlTagKind::Normal),
        ("li", HtmlTagKind::Normal),
        ("br", HtmlTagKind::Void),
        ("code", HtmlTagKind::RawText),
        ("textarea", HtmlTagKind::EscapableRawText),
    ]
    .into_iter()
    .map(|(k, v)| (CaseInsensitive(CaseInsensitive(k.as_bytes())), v))
    .collect()
});

fuzz_target!(|markdown: &str| {
    // Prevent stack overflows from nested blockquotes. We can’t reasonably support this anyway.
    if 2000 < markdown.bytes().filter(|&b| b == b'>').count() {
        return;
    }

    let options = cmarker_typst::Options {
        html_tags: &*HTML_TAGS,
        label_prefix: "",
        label_use_prefix: "",
        heading_label_case: HeadingLabelCase::KebabPreserve,
        flags: Flags::SMART_PUNCTUATION | Flags::MATH,
        h1_level: 1,
    };
    let text = cmarker_typst::run(markdown, options).unwrap();
    let text = String::from_utf8(text).unwrap();
    if let Some(error) = typst_syntax::parse(&text).errors().into_iter().next() {
        panic!("{}", error.message);
    }
});

use cmarker_typst::CaseInsensitive;
use cmarker_typst::Flags;
use cmarker_typst::HeadingLabelCase;
use cmarker_typst::HtmlTagKind;
use cmarker_typst::HtmlTagMap;
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;
