#import "/test-runner/common.typ": run-cmarker

#set heading(numbering: "1.")

#run-cmarker("# h\n[@h]\n[Chapter][@h]\n<h1 id=k></h1>", label-prefix: "p")

#run-cmarker("# h\n[@Xh]\n\n[Chapter][@Xh]", label-prefix: "X", prefix-label-uses: false)

@ph @Xh

@pk

#run-cmarker("# My heading\n[@My-heading]", heading-label-case: "kebab-preserve")

@My-heading
