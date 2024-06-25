**Original pretty-printer**
- Please checkout out the original version of this library  [elm-pretty-printer](https://package.elm-lang.org/packages/the-sett/elm-pretty-printer/latest/)

**Status**

- 25-Jun-2024 - Published as version 1.0.0

A new way of tagging documents has been added to this library.To add a `Tag` to 
a `Doc` we have to use the function `tagged` (add's a opening and closing tag to
a certain document?).It is also possible to use `openTag` and `closeTag` to only
add a closing or opening tag.

# elm-pretty-printer

A pretty printing library based on ['A Prettier Printer' by Philip Wadler](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

This version follows Wadler's paper closely, but is actually ported from a Haskell
implementation that is referred to as Wadler/Leijen. Leijen added Column and Nesting
constructors in the document type, which make for easier and more flexible indentation.

I have added the ability to have different ways of joining documents when placing them
on the same line, or breaking them into multiple lines. When placed within a `group`
, the usual `line` function will render as a line break or a space; the `tightline`
will render as a line break or "". The `separators` function allows a string to be
given that is placed between docs on the same line, or after the start of the line
when placing on multiple lines - useful when rendering commas in a list at the start
of the line.

This implementation is usually sufficiently lazy and tail-recursive to perform well
under Elm. Occassionally an exponential blow-up can happen if the `softline` function
is not used carefully. Plase raise a GitHub issue on this if it is not performing well
enough for you.

# Syntax Highlighting and rendering to HTML

The `Pretty.Renderer` module allows for finer control over how the output is produced.

This requires a `Renderer` specification to be set up that provides
call-back functions that will be used when tagged strings in the document are
encountered, and also at line ends.

This is left folded over the output using whatever accumulator type you need in order to transform the output.

# Example Renderer 
In the file `Test.elm` you can find a definition of a `renderer` that allows the tagging of 
documents that is close to the tagging method in HTML.

> [!NOTE]
> This version allows the same functionality has the original , but it allows the tagging of documents as a whole instead of only tagging Strings. This changes were made to adjust to the need of having a more flexibel control of tags.To be more specific , this library was made to enhance the UI experience of another project named [haskelite](https://github.com/pbv/haskelite) made by Pedro Vasconcelos . 
