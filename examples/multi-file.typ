#import "cmarker/lib.typ" as cmarker

#set heading(numbering: "1.")

An example of splitting your document over multiple Markdown files,
allowing the different files to link to each other.

#cmarker.render(read("multi-file-1.md") + read("multi-file-2.md"))
