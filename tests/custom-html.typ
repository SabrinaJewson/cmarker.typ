#import "/test-runner/common.typ": run-cmarker

#run-cmarker(
  "
<my-normal-element a=1 b=2>3</my-normal-element>

<another-normal-element foo foo=disappear></another-normal-element>,

<void-elem>

<raw-text-elem>
&amp;
</raw-text-elem>

<escapable-raw-text-elem>
&amp;
</escapable-raw-text-elem>
  ",
  html: (
    my-normal-element: (attrs, it) => [#attrs.a #attrs.b #it],
    another-normal-element: ("normal", (attrs, it) => [#attrs]),
    void-elem: ("void", attrs => [void #attrs]),
    raw-text-elem: ("raw-text", (attrs, it) => {
      if type(it) == str {
        [raw-text #it]
      }
    }),
    escapable-raw-text-elem: ("escapable-raw-text", (attrs, it) => {
      if type(it) == str {
        [escapable-raw-text #it]
      }
    }),
  ),
)
