open Angstrom
open Ast

let is_eol = function '\n' | '\r' -> true | _ -> false
let is_special = function '*' | '/' | '=' | '~' | '[' -> true | _ -> false

let merge_text inlines =
  let rec aux acc = function
    | [] -> List.rev acc
    | Text s1 :: Text s2 :: rest -> aux acc (Text (s1 ^ s2) :: rest)
    | x :: rest -> aux (x :: acc) rest
  in
  aux [] inlines

let code =
  char '~' *> take_while1 (fun c -> c <> '~' && not (is_eol c)) <* char '~'
  >>| fun s -> Code s

let verbatim =
  char '=' *> take_while1 (fun c -> c <> '=' && not (is_eol c)) <* char '='
  >>| fun s -> Verbatim s

let link =
  string "[[" *> take_while1 (fun c -> c <> ']' && c <> '[') >>= fun url ->
  string "][" *> take_while1 (fun c -> c <> ']')
  <* string "]]"
  >>| (fun desc -> Link { url; desc = Some [ Text desc ] })
  <|> string "]]" *> return (Link { url; desc = None })

let rec parse_inline stop_c =
  let plain_text =
    take_while1 (fun c ->
        (not (is_special c)) && (not (is_eol c)) && c <> stop_c)
    >>| fun s -> Text s
  in
  let fallback_char =
    peek_char_fail >>= fun c ->
    if c = stop_c then fail "closing delimiter"
    else advance 1 >>| fun () -> Text (String.make 1 c)
  in
  choice
    [
      ( char '*' *> return () >>= fun () ->
        many1 (parse_inline '*') <* char '*' >>| fun children ->
        Bold (merge_text children) );
      ( char '/' *> return () >>= fun () ->
        many1 (parse_inline '/') <* char '/' >>| fun children ->
        Italic (merge_text children) );
      code;
      verbatim;
      link;
      plain_text;
      fallback_char;
    ]

let parser = many (parse_inline '\x00') >>| merge_text

let parse s =
  if String.trim s = "" then [ Text s ]
  else
    match parse_string ~consume:All parser s with
    | Ok inlines -> inlines
    | Error _ -> [ Text s ]
