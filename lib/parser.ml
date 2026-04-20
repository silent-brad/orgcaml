open Ast

let parse input =
  let content = Block_parser.parse input in
  { properties = []; content }
