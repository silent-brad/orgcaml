let input =
  {|#+TITLE: OrgCaml SDK Demo

* Introduction

This document demonstrates *bold*, /italic/, ~code~, and =verbatim= markup,
as well as [[https://orgmode.org][links to Org-mode]].

** TODO Items and Lists

- Parse Org files
- Render to HTML
- Profit

1. First ordered item
2. Second ordered item

** Source Code

#+BEGIN_SRC ocaml
let greet name =
  Printf.printf "Hello, %s!\n" name
#+END_SRC

** Example Output

#+BEGIN_EXAMPLE
$ ./greet world
Hello, world!
#+END_EXAMPLE

** Data Table

| Language | Typing | Compiled |
|----------+--------+----------|
| OCaml    | Strong | Yes      |
| Python   | Weak   | No       |
| Rust     | Strong | Yes      |

-----

#+BEGIN_QUOTE
Simplicity is prerequisite for reliability.
— Edsger W. Dijkstra
#+END_QUOTE
|}

let () =
  let doc = Orgcaml.Parser.parse input in
  print_string (Orgcaml.Html.render_full_page ~title:"OrgCaml Demo" doc)
