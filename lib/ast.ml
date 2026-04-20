type inline =
  | Text of string
  | Bold of inline list
  | Italic of inline list
  | Code of string
  | Verbatim of string
  | Link of { url : string; desc : inline list option }

type list_kind = Ordered | Unordered

type block =
  | Heading of { level : int; keyword : string option; title : inline list }
  | Paragraph of inline list
  | List of list_kind * list_item list
  | Src_block of { language : string option; contents : string }
  | Quote_block of block list
  | Horizontal_rule

and list_item = { bullet : string; contents : block list }

type document = { properties : (string * string) list; content : block list }
