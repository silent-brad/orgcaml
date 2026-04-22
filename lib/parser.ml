open Ast

let parse input =
  let lines = String.split_on_char '\n' input in
  let classified = List.map (fun line -> (line, Block_parser.classify_line line)) lines in
  let properties = Block_parser.extract_directives classified in
  let content = Block_parser.blocks_of_lines classified in
  { properties; content }

let languages doc =
  List.filter_map
    (fun block ->
      match block with Src_block { language; _ } -> language | _ -> None)
    doc.content
