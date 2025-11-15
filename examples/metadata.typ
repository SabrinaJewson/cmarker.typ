#import "cmarker/lib.typ" as cmarker

#let (meta, body) = cmarker.render-with-metadata(
  read("metadata.md"),
  metadata-block: "frontmatter-yaml"
)

#set document(title: meta.at("title"))
#set text(font: meta.at("font"))

#heading(meta.at("title"))

#body
