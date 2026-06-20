#import "cmarker/lib.typ" as cmarker
#import "@preview/cheq:0.3.1"

#cmarker.render(
  ```md
  - [ ] Not checked
  - [x] Checked
  ```,
  task-list-marker: checked => if checked { cheq.checked-sym() } else { cheq.unchecked-sym() },
)
