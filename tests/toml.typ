#import "/test-runner/common.typ": run-cmarker-with-metadata, run-cmarker

#let test-template(markdown, expects-metadata: none) = {
  let (meta-raw, body) = run-cmarker-with-metadata(markdown, metadata-block: none)
  assert(run-cmarker(markdown) == body, message: "`cmarker.render(markdown)` should be equal to the body of `cmarker.render-with-metadata(markdown, metadata-block: none)`")

  let (meta-raw, body) = run-cmarker-with-metadata(markdown, metadata-block: "frontmatter-pluses")
  if expects-metadata == none {
    assert(meta-raw == "", message: meta-raw + "!= \"\"")
  } else {
    assert(meta-raw == expects-metadata, message: meta-raw + "!= \"" + expects-metadata + "\"")
  }

  let (meta-toml, body) = run-cmarker-with-metadata(markdown, metadata-block: "frontmatter-toml")
  if expects-metadata == none {
    assert(meta-toml == (:), message: "Expects `meta-toml == (:)` but got type(meta-toml) == " + str(type(meta-toml)))
  } else {
    assert(type(meta-toml) == dictionary, message: "Expects `type(meta-toml) == dictionary` but got type(meta-toml) == " + str(type(meta-toml)))
    assert(meta-toml == toml(bytes(expects-metadata)),
      message: meta-toml.pairs().map(x => "(" + x.at(0) + " = " + x.at(1) + ")").first()
                  + " != "
                  + toml(bytes(expects-metadata)).pairs().map(x => "(" + x.at(0) + " = " + x.at(1) + ")").first()
    )
  }

  divider()
  meta-raw
  divider()

  body
}

#let md = (
  "content with no metadata",
)

#test-template(md.join("\n"), expects-metadata: none)

#let md = (
  "+++",
  "title = 'metadata-test'",
  "+++",
  "content with simple metadata"
)

#test-template(md.join("\n"), expects-metadata: "title = 'metadata-test'")

// simple metadata, whitespace, and no content after
#let md = (
  "+++\t \r",
  "title = 'metadata-test'",
  "+++ \r\t",
)

#test-template(md.join("\n"), expects-metadata: "title = 'metadata-test'")

#let md = (
  "+++",
  "title = 'metadata-test'",
  "+++",
  "content with multiple closing delimiters (only the first is considered)",
  "+++",
  "content with multiple closing delimiters (only the first is considered)"
)

#test-template(md.join("\n"), expects-metadata: "title = 'metadata-test'")

#let md = (
  "+++",
  "\t",
  "title = 'metadata-test'",
  "+++",
  "content that starts with \"---\\n\\n\" has no metadata-block",
)

#test-template(md.join("\n"), expects-metadata: none)

#let md = (
  "+++",
  "title = 'metadata-test'",
  "content that with no closing delimiter has no metadata-block",
)

#test-template(md.join("\n"), expects-metadata: none)
