#import "../lib.typ" as embed-markdown

#embed-markdown.render(
  read("simple.md"),
  smart-punctuation: true,
  blockquote: c => box(stroke: (left: 1pt + black), inset: (left: 5pt, y: 6pt), c),
  show-source: false,
  raw-typst: true,
)
