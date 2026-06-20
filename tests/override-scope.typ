#import "/test-runner/common.typ": run-cmarker

#run-cmarker("*a* **b**", scope: (emph: strong, strong: emph))

#run-cmarker("<em>a</em> <strong>b</strong>", scope: (emph: strong, strong: emph))

#show divider: it => "line-rule"
#run-cmarker("---\n<hr>")

#run-cmarker("---\n<hr>", scope: (rule: () => "rule-rule"))
