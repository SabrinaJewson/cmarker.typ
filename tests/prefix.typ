#import "/test-runner/common.typ": run-cmarker

#set heading(numbering: "1.")

#run-cmarker("# h\n[@h]\n[Chapter][@h]\n<h1 id=k></h1>", label-prefix: "p")

#run-cmarker("# h\n[@xh]\n\n[Chapter][@xh]", label-prefix: "x", prefix-label-uses: false)

@ph @xh

@pk
