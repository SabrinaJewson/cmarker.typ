<!-- SVG -->

1. [ ] not checked
1. 2.
1. [x] is checked
	1. 1.
	1. [x] is checked

<!--raw-typst #set enum(full: true, numbering: "a.") -->

5. [ ] not checked
1. f.
1. [x] checked
	1. g.a.
	1. g.b.
	1. g.c.
	1. [x] checked
+ [ ] not checked
+
+ [x] checked

- •
	- ▸
		- \-
			- •
			- [ ] not checked
		- \-
	- [ ] not checked
- •

reversed:
<!--raw-typst #set enum(reversed: true) -->

3. [ ] not checked
1. [x] checked
1. one
1. zero
1. [ ] not checked

`list.marker` is content:
<!--raw-typst #set list(marker: [🙂]) -->

- 🙂
- 🙂
	- [x] checked
	- 🙂
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
