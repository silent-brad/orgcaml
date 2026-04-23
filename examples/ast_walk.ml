open Orgcaml

let indent n = String.make (n * 2) ' '

let rec print_inline = function
  | Ast.Text s -> Printf.printf "%S" s
  | Ast.Bold children ->
      Printf.printf "Bold(";
      List.iter print_inline children;
      Printf.printf ")"
  | Ast.Italic children ->
      Printf.printf "Italic(";
      List.iter print_inline children;
      Printf.printf ")"
  | Ast.Code s -> Printf.printf "Code(%S)" s
  | Ast.Verbatim s -> Printf.printf "Verbatim(%S)" s
  | Ast.Link { url; desc = Some children } ->
      Printf.printf "Link(%s, " url;
      List.iter print_inline children;
      Printf.printf ")"
  | Ast.Link { url; desc = None } -> Printf.printf "Link(%s)" url

let rec print_block depth = function
  | Ast.Heading { level; keyword; title } ->
      Printf.printf "%sHeading(level=%d" (indent depth) level;
      (match keyword with Some kw -> Printf.printf ", kw=%s" kw | None -> ());
      Printf.printf ", title=";
      List.iter print_inline title;
      Printf.printf ")\n"
  | Ast.Paragraph inlines ->
      Printf.printf "%sParagraph(" (indent depth);
      List.iter print_inline inlines;
      Printf.printf ")\n"
  | Ast.List (kind, items) ->
      let kind_str =
        match kind with
        | Ast.Ordered -> "Ordered"
        | Ast.Unordered -> "Unordered"
      in
      Printf.printf "%sList(%s)\n" (indent depth) kind_str;
      List.iter
        (fun item ->
          Printf.printf "%sItem:\n" (indent (depth + 1));
          List.iter (print_block (depth + 2)) item.Ast.contents)
        items
  | Ast.Src_block { language; contents } ->
      Printf.printf "%sSrc_block(lang=%s, %d bytes)\n" (indent depth)
        (Option.value ~default:"none" language)
        (String.length contents)
  | Ast.Example_block contents ->
      Printf.printf "%sExample_block(%d bytes)\n" (indent depth)
        (String.length contents)
  | Ast.Quote_block blocks ->
      Printf.printf "%sQuote_block:\n" (indent depth);
      List.iter (print_block (depth + 1)) blocks
  | Ast.Table { header; rows } ->
      Printf.printf "%sTable(header=%b, %d rows)\n" (indent depth)
        (Option.is_some header) (List.length rows)
  | Ast.Horizontal_rule -> Printf.printf "%sHorizontal_rule\n" (indent depth)
  | Ast.Comment text -> Printf.printf "%sComment(%s)\n" (indent depth) text

let input =
  {|* Hello *World*

A paragraph with /italic/ and ~code~.

| Name  | Role      |
|-------+-----------|
| Alice | Developer |
| Bob   | Designer  |

#+BEGIN_EXAMPLE
some example output
#+END_EXAMPLE
|}

let () =
  let doc = Parser.parse input in
  Printf.printf "Document: %d top-level blocks\n\n"
    (List.length doc.Ast.content);
  List.iter (print_block 0) doc.Ast.content
