<!-- SVG -->

<!--raw-typst #set enum(full: true, numbering: "a.") -->

5. [ ] not checked
1. item f
1. [x] checked
	1. a list
	1. that
	1. is nested
	1. [x] checked
+ [ ] not checked
+
+ [x] checked

normal nesting:

- a
	- b
		- c
			- d
			- e
		- f
	- g
- h

task list nesting
(this renders slightly incorrectly but there is little to be done):

- a
	- b
		- c
			- d
			- [ ] e
		- [ ] f
	- [ ] g
- h

reversed:
<!--raw-typst #set enum(reversed: true) -->

3. [ ] not checked
1. [x] checked
1. one
1. zero
1. [ ] does not display a checkbox (unavoidable limitation)

`list.marker` is content:
<!--raw-typst #set list(marker: [🙂]) -->

- foo
- bar
	- baz
- [ ] not checked

`list.marker` is a function:
<!--raw-typst #set list(marker: n => [#n]) -->

- zero
- [ ] not checked
	- [ ] not checked
	- one
		- [ ] not checked
		- two
			- [ ] not checked
			- three
