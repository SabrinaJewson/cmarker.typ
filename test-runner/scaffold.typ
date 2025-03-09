#import "/lib.typ" as cmarker

// Big limitations:
// - Tables
// - Footnotes
// - Links to footnotes

#show raw: it => {
  let attrs = (:)
  if it.lang != none {
    attrs.class = "language-" + it.lang
  }
  if it.block {
    html.elem("pre", attrs: attrs, it.text)
  } else {
    html.elem("code", attrs: attrs, it.text)
  }
}
#show line.where(length: 100%): it => html.elem("hr")
#show image: it => {
  let attrs = (src: it.source)
  if it.alt != none {
    attrs.alt = it.alt
  }
  html.elem("img", attrs: attrs)
}
#show super: it => html.elem("sup", it.body)
#show sub: it => html.elem("sub", it.body)
#show strike: it => html.elem("s", it.body)
#show highlight: it => html.elem("mark", it.body)

#{
  let args = (
    blockquote: body => html.elem("blockquote", body),
    raw-typst: true,
    math: (text, block: true) => raw(block: block, text, lang: "math"),
    scope: (image: (path, alt: none) => image(path, alt: alt)),
  )
  if "show-source" in sys.inputs {
    let source = cmarker.render(read(sys.inputs.md), show-source: true, ..args).text
    "SOURCESTART"
    for byte in bytes(source) {
      if byte < 0x10 {
        "0"
      }
      str(byte, base: 16)
    }
    "SOURCEEND"
  } else {
    cmarker.render(read(sys.inputs.md), ..args)
  }
}
