#import "common.typ": run-cmarker

#set heading(numbering: "1.")

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
#show footnote: it => {
  if type(it.body) == label {
    it
  } else {
    html.elem("div", attrs: (class: "footnote-definition"), it.body)
  }
}
#show table: it => {
  if type(it.align) != array {
    return it
  }
  html.elem("div", attrs: (data-alignments: { it.align.map(a => [#a].text).join(",") }), it)
}
#show link: it => {
  if type(it.dest) != label {
    return it
  }
  html.elem("span", attrs: (data-dest: str(it.dest)), it)
}

#{
  let args = (
    raw-typst: true,
    math: (text, block: true) => raw(block: block, text, lang: "math"),
    scope: (
      image: (path, alt: none) => image(path, alt: alt),
      rule: () => html.elem("hr"),
    ),
  )
  run-cmarker(read(sys.inputs.md), ..args)
}
