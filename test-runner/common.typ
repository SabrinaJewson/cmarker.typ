#import "/lib.typ" as cmarker

#let run-cmarker-with-metadata(..args) = {
  if "show-source" in sys.inputs {
    let (meta, source) = cmarker.render-with-metadata(show-source: true, ..args)
    let source = {
      "SOURCESTART"
      for byte in bytes(source.text) {
        if byte < 0x10 {
          "0"
        }
        str(byte, base: 16)
      }
      "SOURCEEND"
    }
    (meta, source)
  } else {
    cmarker.render-with-metadata(..args)
  }
}

#let run-cmarker(..args) = {
  if "show-source" in sys.inputs {
    let source = cmarker.render(show-source: true, ..args)
    "SOURCESTART"
    for byte in bytes(source.text) {
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
