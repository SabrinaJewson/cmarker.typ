#import "/test-runner/common.typ": run-cmarker

#set heading(numbering: "1.")

#run-cmarker("# h\n[@h]\n[Chapter][@h]\n<h1 id=k></h1>", label-prefix: "p")

#run-cmarker("# h\n[@X'h]\n\n[Chapter][@X'h]", label-prefix: "X'", prefix-label-uses: false)

@ph #ref(label("X'h"))

@pk

#run-cmarker("# My heading\n[@My-heading]", heading-labels: "jupyter")

@My-heading
