#import "cmarker/lib.typ" as cmarker

#set heading(numbering: "1.")

An example of splitting your document over multiple Markdown files,
and overriding the `link` function to allow the files to link to each other.

#let scope = (
  link: (dest, body) => {
    if dest.starts-with("https://") or dest.starts-with("http://") {
      return link(dest, body)
    }
    ref(label(dest), supplement: if body == [] { none } else { (el) => body })
  }
)

#cmarker.render(read("multi-file-1.md"), raw-typst: true, scope: scope)
#cmarker.render(read("multi-file-2.md"), raw-typst: true, scope: scope)
