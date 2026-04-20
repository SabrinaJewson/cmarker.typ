#import "/test-runner/common.typ": run-cmarker

#run-cmarker("# a\n# b\n# c", h1-level: 20)

#run-cmarker("# a\n## b\n### c", h1-level: -1)

#box[
  #run-cmarker("# not title", h1-level: 0, set-document-title: false)
]
