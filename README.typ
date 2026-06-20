#import "lib.typ" as cmarker

#set page(numbering: "1", number-align: center)
#set text(lang: "en")
#set heading(numbering: "1.")

#show link: c => text(underline(c), fill: blue)
#show title: t => align(center)[#t]

#cmarker.render(read("README.md"), h1-level: 0, smart-punctuation: false)

#context {
  for elem in query(raw.where(lang: "typst")) {
    assert(false, message: "use ```typ instead of ```typst")
  }
  for elem in query(raw.where(lang: "typ")) {
    if elem.text.starts-with("#import \"@preview/cmarker:") {
      continue
    }
    let text = elem.text.replace("read(\"example.md\")", "\"\"")
    let _ = eval(text, mode: "markup", scope: (cmarker: cmarker))
  }
}
