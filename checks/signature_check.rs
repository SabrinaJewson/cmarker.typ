#[test]
fn test() {
    let lib_typ = fs::read_to_string("../lib.typ").unwrap();
    let readme = fs::read_to_string("../README.md").unwrap();

    let with_metadata = between(&lib_typ, "\n#let render-with-metadata(\n", "\n)").unwrap();
    let render = between(&lib_typ, "\n#let render(\n", "\n)").unwrap();

    assert_eq!(
        with_metadata
            .strip_suffix("\n  metadata-block: none,")
            .unwrap(),
        render,
    );

    let readme_render = between(&readme, "\nrender(\n", "\n)").unwrap();
    assert_eq!(readme_render, render);

    let readme_with_metadata = between(&readme, "\nrender-with-metadata(\n", "\n)").unwrap();
    assert_eq!(readme_with_metadata, with_metadata);

    let params = with_metadata
        .lines()
        .map(|l| l.split(':').next().unwrap().trim_matches([' ', ',']))
        .collect::<Vec<_>>();

    let param_headers = between(&readme, "\n## API\n", "\n## ")
        .unwrap()
        .lines()
        .filter_map(|l| l.strip_prefix("#### "))
        .map(|p| p.trim_matches('`'))
        .collect::<Vec<_>>();

    assert_eq!(params, param_headers);
}

fn between<'a>(s: &'a str, before: &str, after: &str) -> Option<&'a str> {
    let (_, s) = s.split_once(before).unwrap();
    let (s, _) = s.split_once(after).unwrap();
    Some(s)
}

use std::fs;
