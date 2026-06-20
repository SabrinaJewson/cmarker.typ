#[test]
fn test() {
    let typst = fs::read_to_string("../typst.toml").unwrap();
    let typst = typst
        .lines()
        .find_map(|l| l.strip_prefix("compiler = "))
        .unwrap();

    let cargo = fs::read_to_string("../Cargo.toml").unwrap();
    let cargo = cargo
        .lines()
        .find_map(|l| l.strip_prefix("typst-syntax = "))
        .unwrap();

    let ci = fs::read_to_string("../.github/workflows/ci.yml").unwrap();
    let ci = ci
        .lines()
        .find_map(|l| l.trim().strip_prefix("typst-version: "))
        .unwrap();

    if typst == cargo && cargo == ci {
        return;
    }

    println!("  typst.toml:               {typst}");
    println!("  Cargo.toml:               {cargo}");
    println!("  .github/workflows/ci.yml: {ci}");
    panic!();
}

use std::fs;
