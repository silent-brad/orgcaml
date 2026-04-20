let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let () =
  let usage = "Usage: orgcaml [--full-page] <input.org>" in
  let args = Array.to_list Sys.argv |> List.tl in
  let full_page, files = List.partition (fun s -> s = "--full-page") args in
  let full_page = full_page <> [] in
  match files with
  | [ path ] ->
      let input = read_file path in
      let doc = Orgcaml.Parser.parse input in
      let html =
        if full_page then
          let title = Filename.chop_extension (Filename.basename path) in
          Orgcaml.Html.render_full_page ~title doc
        else Orgcaml.Html.render_document doc
      in
      print_string html
  | _ ->
      prerr_endline usage;
      exit 1
