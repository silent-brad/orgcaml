open Angstrom
open Ast

let is_eol = function '\n' | '\r' -> true | _ -> false

let is_special = function
  | '*' | '/' | '=' | '~' | '[' -> true
  | _ -> false

let delimited open_c close_c f =
  char open_c *>
  take_while1 (fun c -> c <> close_c && not (is_eol c)) <*
  char close_c >>| f

let bold = delimited '*' '*' (fun s -> Bold [Text s])
let italic = delimited '/' '/' (fun s -> Italic [Text s])
let code = delimited '~' '~' (fun s -> Code s)
let verbatim = delimited '=' '=' (fun s -> Verbatim s)

let link =
  string "[[" *>
  take_while1 (fun c -> c <> ']' && c <> '[') >>= fun url ->
  ( string "][" *>
    take_while1 (fun c -> c <> ']') <* string "]]" >>| fun desc ->
    Link { url; desc = Some [Text desc] } )
  <|>
  ( string "]]" *> return (Link { url; desc = None }) )

let plain_text =
  take_while1 (fun c -> not (is_special c) && not (is_eol c)) >>| fun s ->
  Text s

let fallback_char =
  any_char >>| fun c -> Text (String.make 1 c)

let inline_element =
  choice [ bold; italic; code; verbatim; link; plain_text; fallback_char ]

let merge_text inlines =
  let rec aux acc = function
    | [] -> List.rev acc
    | Text s1 :: Text s2 :: rest -> aux acc (Text (s1 ^ s2) :: rest)
    | x :: rest -> aux (x :: acc) rest
  in
  aux [] inlines

let parser = many inline_element >>| merge_text

let parse s =
  if String.trim s = "" then [Text s]
  else
    match parse_string ~consume:All parser s with
    | Ok inlines -> inlines
    | Error _ -> [Text s]
