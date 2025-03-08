#import "lib.typ" as cmarker

#set document(title: "cmarker.typ")
#set page(numbering: "1", number-align: center)
#set text(lang: "en")
#set heading(numbering: "1.")

#show link: c => text(underline(c), fill: blue)

#align(center, text(weight: 700, 1.75em)[cmarker.typ])
#align(center)[https://github.com/SabrinaJewson/cmarker.typ]

#cmarker.render(
  read("README.md"),
  h1-level: 0,
  smart-punctuation: false,
  raw-typst: true,
)
