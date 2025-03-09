#![cfg_attr(not(feature = "std"), no_std)]
#![allow(clippy::items_after_test_module)]

extern crate alloc;

#[global_allocator]
static ALLOC: GlobalDlmalloc = GlobalDlmalloc;

#[panic_handler]
#[cfg(target_arch = "wasm32")]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

wasm_minimal_protocol::initiate_protocol!();

#[wasm_func]
fn render(markdown: &[u8], options: &[u8]) -> Result<Vec<u8>, String> {
    let &[options, h1_level, ref html_tags_bytes @ ..] = options else {
        panic!()
    };
    let options = cmarker_typst::Options::from_bits(options).unwrap();
    let markdown = str::from_utf8(markdown).unwrap();

    let mut html_tags: cmarker_typst::HtmlTags<'_> = HashMap::new();
    for data in html_tags_bytes.split_inclusive(|&n| n >= 0xFC) {
        let (kind, tag) = data.split_last().unwrap();
        let kind = match kind {
            0xFC => cmarker_typst::HtmlTagKind::Void,
            0xFD => cmarker_typst::HtmlTagKind::RawText,
            0xFE => cmarker_typst::HtmlTagKind::EscapableRawText,
            0xFF => cmarker_typst::HtmlTagKind::Normal,
            _ => panic!(),
        };
        html_tags.insert(CaseInsensitive(CaseInsensitive(tag)), kind);
    }

    cmarker_typst::run(markdown, &html_tags, options, h1_level)
}

use alloc::string::String;
use alloc::string::ToString;
use alloc::vec;
use alloc::vec::Vec;
use cmarker_typst::CaseInsensitive;
use cmarker_typst::hashbrown::HashMap;
use core::str;
use dlmalloc::GlobalDlmalloc;
use wasm_minimal_protocol::wasm_func;
