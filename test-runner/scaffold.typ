#import "common.typ": run-cmarker

#set page(width: auto, height: auto) // for SVG export
#set heading(numbering: "1.")

// Typst does not currently emit alignments for tables, so we insert an inert `<div>` that has the
// alignments.
#show table: it => {
  if type(it.align) == array {
    html.elem("div", attrs: (data-alignments: { it.align.map(a => [#a].text).join(",") }))
  }
  it
}

#{
  let args = (
    raw-typst: true,
    math: (text, block: true) => raw(block: block, text, lang: "math"),
    task-list-marker: b => if b { [v] } else { [x] },
    scope: (
      image: (path, ..args) => image(path, ..args),
      rule: html.hr,
    ),
  )
  run-cmarker(read(sys.inputs.md), ..args)
}
