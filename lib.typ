#let _p = plugin("./plugin.wasm")
#let render(
  markdown,
  smart-punctuation: true,
  blockquote: none,
  math: none,
  h1-level: 1,
  raw-typst: true,
  html: (:),
  scope: (:),
  show-source: false,
) = {
  if type(markdown) == content and markdown.has("text") {
    markdown = markdown.text
  }

  let html-defaults = (
    sub: (attrs, body) => sub(body),
    sup: (attrs, body) => super(body),
    mark: (attrs, body) => highlight(body),
    h1: (attrs, body) => scope.at("heading", default: heading)(level: h1-level + 0, body),
    h2: (attrs, body) => scope.at("heading", default: heading)(level: h1-level + 1, body),
    h3: (attrs, body) => scope.at("heading", default: heading)(level: h1-level + 2, body),
    h4: (attrs, body) => scope.at("heading", default: heading)(level: h1-level + 3, body),
    h5: (attrs, body) => scope.at("heading", default: heading)(level: h1-level + 4, body),
    h6: (attrs, body) => scope.at("heading", default: heading)(level: h1-level + 5, body),
    // TODO: <dl>
    // TODO: <ol>, <ul>
    // TODO: <table>
    hr: ("void", (attrs) => scope.at("line", default: line)(length: 100%)),
    a: (attrs, body) => scope.at("link", default: link)(
      if attrs.href.starts-with("#") { label(attrs.href.slice(1)) } else { attrs.href },
      body,
    ),
    em: (attrs, body) => scope.at("emph", default: emph)(body),
    strong: (attrs, body) => scope.at("strong", default: strong)(body),
    s: (attrs, body) => scope.at("strike", default: strike)(body),
    br: ("void", (attrs) => scope.at("linebreak", default: linebreak)()),
    img: ("void", (attrs) => scope.at("image", default: image)(attrs.src, alt: attrs.at("alt", default: none))),
  )

  let options = 0
  if smart-punctuation {
    options += 0b00000001
  }
  if blockquote != none {
    options += 0b00000010
    scope += (blockquote: blockquote)
    html-defaults += (blockquote: (attrs, body) => blockquote(body))
  }
  if raw-typst {
    options += 0b00000100
  }
  if math != none {
    options += 0b00001000
    scope += (inlinemath: math.with(block: false), displaymath: math.with(block: true))
  }

  let options-bytes = (options, h1-level)

  for (k, v) in html-defaults {
    if k not in html {
      html.insert(k, v)
    }
  }
  scope.html = (:)
  for (tag-name, value) in html {
    let (kind, callback) = if type(value) == function { ("normal", value) } else { value };
    scope.html.insert(tag-name, callback)
    for byte in bytes(tag-name) {
      options-bytes.push(byte)
    }
    options-bytes.push(if kind == "void" {
      0xFC
    } else if kind == "raw-text" {
      0xFD
    } else if kind == "escapable-raw-text" {
      0xFE
    } else if kind == "normal" {
      0xFF
    })
  }

  let rendered = str(_p.render(bytes(markdown), bytes(options-bytes)))
  if show-source {
    raw(rendered, block: true, lang: "typ")
  } else {
    eval(rendered, mode: "markup", scope: scope)
  }
}
