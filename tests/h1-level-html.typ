#import "/test-runner/common.typ": run-cmarker

#set heading(numbering: "1.")

#run-cmarker("<h1>crow</h1><h2 id=x>seaweed</h2>", h1-level: 0)

#run-cmarker("<h6>raven</h6>", h1-level: -7)

@x
