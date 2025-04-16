#import "/test-runner/common.typ": run-cmarker

#run-cmarker("> hi", blockquote: strong)

#run-cmarker("<blockquote>hi</blockquote>", blockquote: strong)

#run-cmarker("<!--raw-typst #quote([not block]) -->", blockquote: strong)
