let test name f =
  Printf.printf "  %s... " name;
  try f (); print_endline "OK"
  with e ->
    Printf.printf "FAIL: %s\n" (Printexc.to_string e)

let assert_eq ~expected ~actual =
  if expected <> actual then
    failwith (Printf.sprintf "\n  expected: %S\n  actual:   %S" expected actual)

let parse_and_render input =
  let doc = Orgcaml.Parser.parse input in
  Orgcaml.Html.render_document doc

let () =
  print_endline "=== Inline parsing ===";

  test "bold" (fun () ->
    assert_eq
      ~expected:"<p><strong>hello</strong></p>\n"
      ~actual:(parse_and_render "*hello*"));

  test "italic" (fun () ->
    assert_eq
      ~expected:"<p><em>world</em></p>\n"
      ~actual:(parse_and_render "/world/"));

  test "code" (fun () ->
    assert_eq
      ~expected:"<p><code>foo</code></p>\n"
      ~actual:(parse_and_render "~foo~"));

  test "verbatim" (fun () ->
    assert_eq
      ~expected:"<p><code class=\"verbatim\">bar</code></p>\n"
      ~actual:(parse_and_render "=bar="));

  test "link with desc" (fun () ->
    assert_eq
      ~expected:"<p><a href=\"https://example.com\">Example</a></p>\n"
      ~actual:(parse_and_render "[[https://example.com][Example]]"));

  test "link without desc" (fun () ->
    assert_eq
      ~expected:"<p><a href=\"https://example.com\">https://example.com</a></p>\n"
      ~actual:(parse_and_render "[[https://example.com]]"));

  test "mixed inline" (fun () ->
    assert_eq
      ~expected:"<p>Hello <strong>bold</strong> and <em>italic</em> world</p>\n"
      ~actual:(parse_and_render "Hello *bold* and /italic/ world"));

  print_endline "\n=== Block parsing ===";

  test "heading" (fun () ->
    assert_eq
      ~expected:"<h1>Title</h1>\n"
      ~actual:(parse_and_render "* Title"));

  test "heading level 3" (fun () ->
    assert_eq
      ~expected:"<h3>Deep</h3>\n"
      ~actual:(parse_and_render "*** Deep"));

  test "heading with TODO" (fun () ->
    assert_eq
      ~expected:"<h1><span class=\"todo TODO\">TODO</span> Fix bug</h1>\n"
      ~actual:(parse_and_render "* TODO Fix bug"));

  test "paragraph" (fun () ->
    assert_eq
      ~expected:"<p>Hello world.</p>\n"
      ~actual:(parse_and_render "Hello world."));

  test "multi-line paragraph" (fun () ->
    assert_eq
      ~expected:"<p>Line one. Line two.</p>\n"
      ~actual:(parse_and_render "Line one.\nLine two."));

  test "horizontal rule" (fun () ->
    assert_eq
      ~expected:"<hr />\n"
      ~actual:(parse_and_render "-----"));

  test "unordered list" (fun () ->
    assert_eq
      ~expected:"<ul>\n<li>Alpha</li>\n<li>Beta</li>\n</ul>\n"
      ~actual:(parse_and_render "- Alpha\n- Beta"));

  test "ordered list" (fun () ->
    assert_eq
      ~expected:"<ol>\n<li>First</li>\n<li>Second</li>\n</ol>\n"
      ~actual:(parse_and_render "1. First\n2. Second"));

  test "src block" (fun () ->
    assert_eq
      ~expected:"<pre><code class=\"language-python\">print(&quot;hello&quot;)</code></pre>\n"
      ~actual:(parse_and_render "#+BEGIN_SRC python\nprint(\"hello\")\n#+END_SRC"));

  test "quote block" (fun () ->
    assert_eq
      ~expected:"<blockquote>\n<p>Wise words.</p>\n</blockquote>\n"
      ~actual:(parse_and_render "#+BEGIN_QUOTE\nWise words.\n#+END_QUOTE"));

  test "full document" (fun () ->
    let input = String.concat "\n" [
      "* Hello World";
      "";
      "This is a paragraph with *bold* text.";
      "";
      "- Item one";
      "- Item two";
      "";
      "#+BEGIN_SRC ocaml";
      "let () = print_endline \"hello\"";
      "#+END_SRC";
    ] in
    let html = parse_and_render input in
    assert (String.length html > 0);
    assert (String.sub html 0 4 = "<h1>");
    print_endline "(content OK)");

  print_endline "\nAll tests complete."
