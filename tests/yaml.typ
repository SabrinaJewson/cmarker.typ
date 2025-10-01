#import "/test-runner/common.typ": run-cmarker-with-metadata, run-cmarker

#let test-template(markdown, expects-metadata: none) = {
  let (meta_raw, body) = run-cmarker-with-metadata(markdown, metadata-block: none)
  assert(run-cmarker(markdown) == body, message: "`cmarker.render(markdown)` should be equal to the body of `cmarker.render-with-metadata(markdown, metadata-block: none)`")

  let (meta_raw, body) = run-cmarker-with-metadata(markdown, metadata-block: "frontmatter-raw")
  if expects-metadata == none {
    assert(meta_raw == "", message: meta_raw + "!= \"\"")
  } else {
    assert(meta_raw == expects-metadata, message: meta_raw + "!= \"" + expects-metadata + "\"")
  }

  let (meta_yaml, body) = run-cmarker-with-metadata(markdown, metadata-block: "frontmatter-yaml")
  if expects-metadata == none {
    assert(meta_yaml == none, message: "Expects `meta_yaml == none` but got type(meta_yaml) == " + str(type(meta_raw)))
  } else {
    assert(type(meta_yaml) == dictionary, message: "Expects `type(meta_yaml) == dictionary` but got type(meta_yaml) == " + str(type(meta_raw)))
    assert(meta_yaml == yaml(bytes(expects-metadata)),
      message: meta_yaml.pairs().map(x => "(" + x.at(0) + ": " + x.at(1) + ")").first()
                  + " != "
                  + yaml(bytes(expects-metadata)).pairs().map(x => "(" + x.at(0) + ": " + x.at(1) + ")").first()
    )
  }

  html.elem("hr")
  meta_raw
  html.elem("hr")

  body
}

#let md = (
  "content with no metadata",
)

#test-template(md.join("\n"), expects-metadata: none)

#let md = (
  "---",
  "title: 'metadata-test'",
  "---",
  "content with simple metadata"
)

#test-template(md.join("\n"), expects-metadata: "title: 'metadata-test'")

#let md = (
  "---",
  "title: 'metadata-test'",
  "...",
  "content with simple metadata and `...` closing delimiter"
)

#test-template(md.join("\n"), expects-metadata: "title: 'metadata-test'")

#let md = (
  "---",
  "title: 'metadata-test'",
  "---",
  "content with multiple closing delimiters (only the first is considered)",
  "...",
  "content with multiple closing delimiters (only the first is considered)"
)

#test-template(md.join("\n"), expects-metadata: "title: 'metadata-test'")

#let md = (
  "---",
  "",
  "title: 'metadata-test'",
  "---",
  "content that starts with \"---\\n\\n\" has no metadata-block",
)

#test-template(md.join("\n"), expects-metadata: none)

#let md = (
  "---",
  "title: 'metadata-test'",
  "content that with no closing delimiter has no metadata-block",
)

#test-template(md.join("\n"), expects-metadata: none)
