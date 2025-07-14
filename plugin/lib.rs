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
    let &[flags, h1_level, heading_labels, ref rest @ ..] = options else {
        panic!()
    };
    let mut parts = rest
        .split_inclusive(|&n| n >= 0xFC)
        .map(|s| s.split_last().unwrap());
    let (_, label_prefix) = parts.next().unwrap();
    let (_, label_use_prefix) = parts.next().unwrap();

    let flags = cmarker_typst::Flags::from_bits(flags).unwrap();
    let markdown = str::from_utf8(markdown).unwrap();
    let label_prefix = str::from_utf8(label_prefix).unwrap();
    let label_use_prefix = str::from_utf8(label_use_prefix).unwrap();

    let mut html_tags: cmarker_typst::HtmlTagMap<'_> = HashMap::new();
    for (kind, tag) in parts {
        let kind = match kind {
            0xFC => cmarker_typst::HtmlTagKind::Void,
            0xFD => cmarker_typst::HtmlTagKind::RawText,
            0xFE => cmarker_typst::HtmlTagKind::EscapableRawText,
            0xFF => cmarker_typst::HtmlTagKind::Normal,
            _ => panic!(),
        };
        html_tags.insert(CaseInsensitive(CaseInsensitive(tag)), kind);
    }

    let options = cmarker_typst::Options {
        html_tags: &html_tags,
        label_prefix,
        label_use_prefix,
        heading_labels: cmarker_typst::HeadingLabels::from_u8(heading_labels).unwrap(),
        flags,
        h1_level,
    };

    cmarker_typst::run(markdown, options)
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
