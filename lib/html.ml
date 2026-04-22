open Ast

let escape_html s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let image_exts = [ ".png"; ".jpg"; ".jpeg"; ".gif"; ".svg"; ".webp"; ".bmp" ]

let is_image_url url =
  let lower = String.lowercase_ascii url in
  List.exists (fun ext ->
    let elen = String.length ext in
    let ulen = String.length lower in
    ulen >= elen && String.sub lower (ulen - elen) elen = ext) image_exts

let strip_file_prefix url =
  let prefix = "file:" in
  let plen = String.length prefix in
  if String.length url >= plen && String.sub url 0 plen = prefix then
    String.sub url plen (String.length url - plen)
  else url

let rec render_inline buf = function
  | Text s -> Buffer.add_string buf (escape_html s)
  | Bold children ->
      Buffer.add_string buf "<strong>";
      List.iter (render_inline buf) children;
      Buffer.add_string buf "</strong>"
  | Italic children ->
      Buffer.add_string buf "<em>";
      List.iter (render_inline buf) children;
      Buffer.add_string buf "</em>"
  | Code s ->
      Buffer.add_string buf "<code>";
      Buffer.add_string buf (escape_html s);
      Buffer.add_string buf "</code>"
  | Verbatim s ->
      Buffer.add_string buf "<code class=\"verbatim\">";
      Buffer.add_string buf (escape_html s);
      Buffer.add_string buf "</code>"
  | Link { url; desc = Some children } ->
      let clean_url = strip_file_prefix url in
      if is_image_url clean_url then (
        Buffer.add_string buf "<img src=\"";
        Buffer.add_string buf (escape_html clean_url);
        Buffer.add_string buf "\" alt=\"";
        List.iter (render_inline buf) children;
        Buffer.add_string buf "\" />")
      else (
        Buffer.add_string buf "<a href=\"";
        Buffer.add_string buf (escape_html clean_url);
        Buffer.add_string buf "\">";
        List.iter (render_inline buf) children;
        Buffer.add_string buf "</a>")
  | Link { url; desc = None } ->
      let clean_url = strip_file_prefix url in
      if is_image_url clean_url then (
        Buffer.add_string buf "<img src=\"";
        Buffer.add_string buf (escape_html clean_url);
        Buffer.add_string buf "\" alt=\"";
        Buffer.add_string buf (escape_html (Filename.basename clean_url));
        Buffer.add_string buf "\" />")
      else (
        Buffer.add_string buf "<a href=\"";
        Buffer.add_string buf (escape_html clean_url);
        Buffer.add_string buf "\">";
        Buffer.add_string buf (escape_html clean_url);
        Buffer.add_string buf "</a>")

let rec render_block buf = function
  | Heading { level; keyword; title } ->
      let tag = Printf.sprintf "h%d" (min level 6) in
      Buffer.add_string buf (Printf.sprintf "<%s>" tag);
      (match keyword with
      | Some kw ->
          Buffer.add_string buf
            (Printf.sprintf "<span class=\"todo %s\">%s</span> " kw kw)
      | None -> ());
      List.iter (render_inline buf) title;
      Buffer.add_string buf (Printf.sprintf "</%s>\n" tag)
  | Paragraph inlines ->
      Buffer.add_string buf "<p>";
      List.iter (render_inline buf) inlines;
      Buffer.add_string buf "</p>\n"
  | List (kind, items) ->
      let tag = match kind with Ordered -> "ol" | Unordered -> "ul" in
      Buffer.add_string buf (Printf.sprintf "<%s>\n" tag);
      List.iter
        (fun item ->
          Buffer.add_string buf "<li>";
          List.iter (render_block_inline buf) item.contents;
          Buffer.add_string buf "</li>\n")
        items;
      Buffer.add_string buf (Printf.sprintf "</%s>\n" tag)
  | Src_block { language; contents } ->
      let lang_attr =
        match language with
        | Some l -> Printf.sprintf " class=\"language-%s\"" (escape_html l)
        | None -> ""
      in
      Buffer.add_string buf (Printf.sprintf "<pre><code%s>" lang_attr);
      Buffer.add_string buf (escape_html contents);
      Buffer.add_string buf "</code></pre>\n"
  | Example_block contents ->
      Buffer.add_string buf "<pre class=\"example\">";
      Buffer.add_string buf (escape_html contents);
      Buffer.add_string buf "</pre>\n"
  | Quote_block blocks ->
      Buffer.add_string buf "<blockquote>\n";
      List.iter (render_block buf) blocks;
      Buffer.add_string buf "</blockquote>\n"
  | Table { header; rows } ->
      Buffer.add_string buf "<table>\n";
      (match header with
      | Some cells ->
          Buffer.add_string buf "<thead>\n<tr>";
          List.iter
            (fun cell ->
              Buffer.add_string buf "<th>";
              List.iter (render_inline buf) cell;
              Buffer.add_string buf "</th>")
            cells;
          Buffer.add_string buf "</tr>\n</thead>\n"
      | None -> ());
      if rows <> [] then begin
        Buffer.add_string buf "<tbody>\n";
        List.iter
          (fun row ->
            Buffer.add_string buf "<tr>";
            List.iter
              (fun cell ->
                Buffer.add_string buf "<td>";
                List.iter (render_inline buf) cell;
                Buffer.add_string buf "</td>")
              row;
            Buffer.add_string buf "</tr>\n")
          rows;
        Buffer.add_string buf "</tbody>\n"
      end;
      Buffer.add_string buf "</table>\n"
  | Horizontal_rule -> Buffer.add_string buf "<hr />\n"

(* For list items, render paragraph contents inline (no <p> wrapper) *)
and render_block_inline buf = function
  | Paragraph inlines -> List.iter (render_inline buf) inlines
  | other -> render_block buf other

let render_document doc =
  let buf = Buffer.create 4096 in
  List.iter (render_block buf) doc.content;
  Buffer.contents buf

let render_full_page ~title doc =
  let buf = Buffer.create 8192 in
  Buffer.add_string buf "<!DOCTYPE html>\n<html>\n<head>\n";
  Buffer.add_string buf
    (Printf.sprintf "<meta charset=\"utf-8\">\n<title>%s</title>\n"
       (escape_html title));
  Buffer.add_string buf "</head>\n<body>\n";
  Buffer.add_string buf (render_document doc);
  Buffer.add_string buf "</body>\n</html>\n";
  Buffer.contents buf
