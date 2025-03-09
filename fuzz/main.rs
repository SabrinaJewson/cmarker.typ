#![no_main]

static HTML_TAGS: LazyLock<HtmlTags<'static>> = LazyLock::new(|| {
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
    let options = Options::SMART_PUNCTUATION | Options::BLOCKQUOTE | Options::MATH;
    let text = cmarker_typst::run(markdown, &HTML_TAGS, options, 1).unwrap();
    let text = String::from_utf8(text).unwrap();
    if let Some(error) = typst_syntax::parse(&text).errors().into_iter().next() {
        panic!("{}", error.message);
    }
});

use cmarker_typst::CaseInsensitive;
use cmarker_typst::HtmlTagKind;
use cmarker_typst::HtmlTags;
use cmarker_typst::Options;
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;
