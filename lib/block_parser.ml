open Ast

(* --- Line classification --- *)

type raw_line =
  | L_heading of int * string
  | L_blank
  | L_hr
  | L_begin_src of string option
  | L_end_src
  | L_begin_quote
  | L_end_quote
  | L_ulist_item of string
  | L_olist_item of string
  | L_text of string

let starts_with prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let string_ci_equal a b =
  String.lowercase_ascii a = String.lowercase_ascii b

let count_leading_char c s =
  let len = String.length s in
  let rec loop i = if i < len && s.[i] = c then loop (i + 1) else i in
  loop 0

let is_digit = function '0' .. '9' -> true | _ -> false

let classify_line line =
  let trimmed = String.trim line in
  let len = String.length trimmed in
  if len = 0 then L_blank
  else
    let stars = count_leading_char '*' trimmed in
    if stars > 0 && stars < len && trimmed.[stars] = ' ' then
      L_heading (stars, String.sub trimmed (stars + 1) (len - stars - 1))
    else if len >= 5 && String.for_all (fun c -> c = '-') trimmed then
      L_hr
    else if string_ci_equal (String.trim trimmed) "#+END_SRC" then
      L_end_src
    else if string_ci_equal (String.trim trimmed) "#+END_QUOTE" then
      L_end_quote
    else if starts_with "#+BEGIN_SRC" (String.uppercase_ascii trimmed)
            || starts_with "#+begin_src" (String.lowercase_ascii trimmed) then
      let after = String.sub trimmed 11 (len - 11) |> String.trim in
      L_begin_src (if after = "" then None else Some after)
    else if starts_with "#+BEGIN_QUOTE" (String.uppercase_ascii trimmed)
            || starts_with "#+begin_quote" (String.lowercase_ascii trimmed) then
      L_begin_quote
    else if len >= 2 && trimmed.[0] = '-' && trimmed.[1] = ' ' then
      L_ulist_item (String.sub trimmed 2 (len - 2))
    else begin
      (* Check for ordered list: digits followed by ". " *)
      let rec scan_digits i =
        if i < len && is_digit trimmed.[i] then scan_digits (i + 1) else i
      in
      let d = scan_digits 0 in
      if d > 0 && d + 1 < len && trimmed.[d] = '.' && trimmed.[d + 1] = ' ' then
        L_olist_item (String.sub trimmed (d + 2) (len - d - 2))
      else
        L_text trimmed
    end

(* --- Block grouping --- *)

let parse_heading raw =
  let parts = String.split_on_char ' ' raw in
  match parts with
  | kw :: rest when kw = "TODO" || kw = "DONE" ->
    (Some kw, Inline_parser.parse (String.concat " " rest))
  | _ ->
    (None, Inline_parser.parse raw)

let rec blocks_of_lines lines =
  match lines with
  | [] -> []
  | L_blank :: rest -> blocks_of_lines rest
  | L_heading (level, raw) :: rest ->
    let keyword, title = parse_heading raw in
    Heading { level; keyword; title } :: blocks_of_lines rest
  | L_hr :: rest ->
    Horizontal_rule :: blocks_of_lines rest
  | L_begin_src lang :: rest ->
    let content_lines, rest = collect_until_end_src rest in
    Src_block { language = lang; contents = String.concat "\n" content_lines }
    :: blocks_of_lines rest
  | L_end_src :: rest ->
    (* Stray end marker, treat as text *)
    Paragraph (Inline_parser.parse "#+END_SRC") :: blocks_of_lines rest
  | L_begin_quote :: rest ->
    let inner_lines, rest = collect_until_end_quote rest in
    let inner_blocks = blocks_of_lines inner_lines in
    Quote_block inner_blocks :: blocks_of_lines rest
  | L_end_quote :: rest ->
    Paragraph (Inline_parser.parse "#+END_QUOTE") :: blocks_of_lines rest
  | L_ulist_item _ :: _ ->
    let items, rest = collect_list_items (fun l ->
      match l with L_ulist_item _ -> true | _ -> false) lines in
    let list_items = List.map (fun text ->
      { bullet = "-"; contents = [Paragraph (Inline_parser.parse text)] }
    ) items in
    List (Unordered, list_items) :: blocks_of_lines rest
  | L_olist_item _ :: _ ->
    let items, rest = collect_list_items (fun l ->
      match l with L_olist_item _ -> true | _ -> false) lines in
    let list_items = List.map (fun text ->
      { bullet = "1."; contents = [Paragraph (Inline_parser.parse text)] }
    ) items in
    List (Ordered, list_items) :: blocks_of_lines rest
  | L_text _ :: _ ->
    let para_lines, rest = collect_paragraph lines in
    let text = String.concat " " para_lines in
    Paragraph (Inline_parser.parse text) :: blocks_of_lines rest

and collect_until_end_src lines =
  match lines with
  | [] -> ([], [])
  | L_end_src :: rest -> ([], rest)
  | line :: rest ->
    let raw = raw_line_text line in
    let more, rest = collect_until_end_src rest in
    (raw :: more, rest)

and collect_until_end_quote lines =
  match lines with
  | [] -> ([], [])
  | L_end_quote :: rest -> ([], rest)
  | line :: rest ->
    let more, final_rest = collect_until_end_quote rest in
    (line :: more, final_rest)

and collect_list_items pred lines =
  match lines with
  | line :: rest when pred line ->
    let text = match line with
      | L_ulist_item t | L_olist_item t -> t
      | _ -> ""
    in
    let more_items, final_rest = collect_list_items pred rest in
    (text :: more_items, final_rest)
  | _ -> ([], lines)

and collect_paragraph lines =
  match lines with
  | L_text t :: rest ->
    let more, final_rest = collect_paragraph rest in
    (t :: more, final_rest)
  | _ -> ([], lines)

and raw_line_text = function
  | L_heading (level, t) -> String.make level '*' ^ " " ^ t
  | L_blank -> ""
  | L_hr -> "-----"
  | L_begin_src _ -> "#+BEGIN_SRC"
  | L_end_src -> "#+END_SRC"
  | L_begin_quote -> "#+BEGIN_QUOTE"
  | L_end_quote -> "#+END_QUOTE"
  | L_ulist_item t -> "- " ^ t
  | L_olist_item t -> "1. " ^ t
  | L_text t -> t

let parse input =
  let lines = String.split_on_char '\n' input in
  let classified = List.map classify_line lines in
  blocks_of_lines classified
