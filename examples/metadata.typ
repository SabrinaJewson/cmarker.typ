#import "cmarker/lib.typ" as cmarker

#cmarker.render(
  read("metadata.md"),
  frontmatter: (meta, body) => {
    meta = yaml(bytes(meta))
    set document(title: meta.at("title"))
    set text(font: meta.at("font"))

    heading(meta.at("title"))

    body
  },
)
