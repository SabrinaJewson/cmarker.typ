#import "/test-runner/common.typ": run-cmarker

#run-cmarker("a \"b\" 'c' -- --- \\\"a\\\" \\'b\\'", smart-punctuation: true)
#run-cmarker("a \"b\" 'c' -- --- \\\"a\\\" \\'b\\'", smart-punctuation: false)

#run-cmarker("> hello\n\nworld")
#run-cmarker("> hello\n\nworld", blockquote: it => html.elem("blockquote", it))

#run-cmarker("$2 + 2$ $$4 + 4$$")
#run-cmarker("$2 + 2$ $$4 + 4$$", math: (block: true, it) => raw(lang: "math", block: block, it))

#run-cmarker("<!--raw-typst .-->", raw-typst: false)
#run-cmarker("<!--raw-typst .-->", raw-typst: true)
