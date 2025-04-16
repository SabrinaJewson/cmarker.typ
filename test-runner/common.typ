#import "/lib.typ" as cmarker

#let run-cmarker(
  markdown,
  smart-punctuation: true,
  math: none,
  h1-level: 1,
  raw-typst: true,
  html: (:),
  scope: (:),
  blockquote: none,
) = {
  let args = arguments(
    markdown,
    smart-punctuation: smart-punctuation,
    math: math,
    h1-level: h1-level,
    raw-typst: raw-typst,
    html: html,
    scope: scope,
    blockquote: blockquote,
  )

  if "show-source" in sys.inputs {
    let source = cmarker.render(show-source: true, ..args).text
    "SOURCESTART"
    for byte in bytes(source) {
      if byte < 0x10 {
        "0"
      }
      str(byte, base: 16)
    }
    "SOURCEEND"
  } else {
    cmarker.render(..args)
  }
}
