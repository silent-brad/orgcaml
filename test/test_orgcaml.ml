let test name f =
  Printf.printf "  %s... " name;
  try
    f ();
    print_endline "OK"
  with e -> Printf.printf "FAIL: %s\n" (Printexc.to_string e)

let assert_eq ~expected ~actual =
  if expected <> actual then
    failwith (Printf.sprintf "\n  expected: %S\n  actual:   %S" expected actual)

let parse_and_render input =
  let doc = Orgcaml.Parser.parse input in
  Orgcaml.Html.render_document doc

let () =
  print_endline "=== Inline parsing ===";

  test "bold" (fun () ->
      assert_eq ~expected:"<p><strong>hello</strong></p>\n"
        ~actual:(parse_and_render "*hello*"));

  test "italic" (fun () ->
      assert_eq ~expected:"<p><em>world</em></p>\n"
        ~actual:(parse_and_render "/world/"));

  test "code" (fun () ->
      assert_eq ~expected:"<p><code>foo</code></p>\n"
        ~actual:(parse_and_render "~foo~"));

  test "verbatim" (fun () ->
      assert_eq ~expected:"<p><code class=\"verbatim\">bar</code></p>\n"
        ~actual:(parse_and_render "=bar="));

  test "link with desc" (fun () ->
      assert_eq ~expected:"<p><a href=\"https://example.com\">Example</a></p>\n"
        ~actual:(parse_and_render "[[https://example.com][Example]]"));

  test "link without desc" (fun () ->
      assert_eq
        ~expected:
          "<p><a href=\"https://example.com\">https://example.com</a></p>\n"
        ~actual:(parse_and_render "[[https://example.com]]"));

  test "mixed inline" (fun () ->
      assert_eq
        ~expected:
          "<p>Hello <strong>bold</strong> and <em>italic</em> world</p>\n"
        ~actual:(parse_and_render "Hello *bold* and /italic/ world"));

  test "double inline" (fun () ->
      assert_eq
        ~expected:
          "<p><strong><code class=\"verbatim\">thing-here</code></strong> \
           <strong><em>bold and italic</em></strong></p>\n"
        ~actual:(parse_and_render "*=thing-here=* */bold and italic/*"));

  print_endline "\n=== Block parsing ===";

  test "heading" (fun () ->
      assert_eq ~expected:"<h1>Title</h1>\n"
        ~actual:(parse_and_render "* Title"));

  test "heading level 3" (fun () ->
      assert_eq ~expected:"<h3>Deep</h3>\n"
        ~actual:(parse_and_render "*** Deep"));

  test "heading with TODO" (fun () ->
      assert_eq
        ~expected:"<h1><span class=\"todo TODO\">TODO</span> Fix bug</h1>\n"
        ~actual:(parse_and_render "* TODO Fix bug"));

  test "paragraph" (fun () ->
      assert_eq ~expected:"<p>Hello world.</p>\n"
        ~actual:(parse_and_render "Hello world."));

  test "multi-line paragraph" (fun () ->
      assert_eq ~expected:"<p>Line one. Line two.</p>\n"
        ~actual:(parse_and_render "Line one.\nLine two."));

  test "horizontal rule" (fun () ->
      assert_eq ~expected:"<hr />\n" ~actual:(parse_and_render "-----"));

  test "unordered list" (fun () ->
      assert_eq ~expected:"<ul>\n<li>Alpha</li>\n<li>Beta</li>\n</ul>\n"
        ~actual:(parse_and_render "- Alpha\n- Beta"));

  test "ordered list" (fun () ->
      assert_eq ~expected:"<ol>\n<li>First</li>\n<li>Second</li>\n</ol>\n"
        ~actual:(parse_and_render "1. First\n2. Second"));

  test "src block" (fun () ->
      assert_eq
        ~expected:
          "<pre><code \
           class=\"language-python\">print(&quot;hello&quot;)</code></pre>\n"
        ~actual:
          (parse_and_render "#+BEGIN_SRC python\nprint(\"hello\")\n#+END_SRC"));

  test "quote block" (fun () ->
      assert_eq ~expected:"<blockquote>\n<p>Wise words.</p>\n</blockquote>\n"
        ~actual:(parse_and_render "#+BEGIN_QUOTE\nWise words.\n#+END_QUOTE"));

  test "example block" (fun () ->
      assert_eq
        ~expected:"<pre class=\"example\">some output\nmore output</pre>\n"
        ~actual:
          (parse_and_render
             "#+BEGIN_EXAMPLE\nsome output\nmore output\n#+END_EXAMPLE"));

  test "example block case insensitive" (fun () ->
      assert_eq ~expected:"<pre class=\"example\">hello</pre>\n"
        ~actual:(parse_and_render "#+begin_example\nhello\n#+end_example"));

  test "table with header" (fun () ->
      assert_eq
        ~expected:
          "<table>\n\
           <thead>\n\
           <tr><th>Name</th><th>Age</th></tr>\n\
           </thead>\n\
           <tbody>\n\
           <tr><td>Alice</td><td>30</td></tr>\n\
           <tr><td>Bob</td><td>25</td></tr>\n\
           </tbody>\n\
           </table>\n"
        ~actual:
          (parse_and_render
             "| Name  | Age |\n\
              |-------+-----|\n\
              | Alice | 30  |\n\
              | Bob   | 25  |"));

  test "table without header" (fun () ->
      assert_eq
        ~expected:
          "<table>\n\
           <tbody>\n\
           <tr><td>a</td><td>b</td></tr>\n\
           <tr><td>c</td><td>d</td></tr>\n\
           </tbody>\n\
           </table>\n"
        ~actual:(parse_and_render "| a | b |\n| c | d |"));

  test "table with inline markup" (fun () ->
      assert_eq
        ~expected:
          "<table>\n\
           <tbody>\n\
           <tr><td><strong>bold</strong></td><td><em>italic</em></td></tr>\n\
           </tbody>\n\
           </table>\n"
        ~actual:(parse_and_render "| *bold* | /italic/ |"));

  test "directives are skipped in output" (fun () ->
      assert_eq ~expected:"<h1>Title</h1>\n"
        ~actual:(parse_and_render "#+TITLE: My Doc\n* Title"));

  test "directives are parsed into properties" (fun () ->
      let doc =
        Orgcaml.Parser.parse "#+TITLE: My Doc\n#+DATE: 2026-04-22\n* Title"
      in
      assert_eq ~expected:"TITLE" ~actual:(fst (List.nth doc.properties 0));
      assert_eq ~expected:"My Doc" ~actual:(snd (List.nth doc.properties 0));
      assert_eq ~expected:"DATE" ~actual:(fst (List.nth doc.properties 1));
      assert_eq ~expected:"2026-04-22" ~actual:(snd (List.nth doc.properties 1)));

  test "image link without desc" (fun () ->
      assert_eq
        ~expected:"<p><img src=\"imgs/photo.png\" alt=\"photo.png\" /></p>\n"
        ~actual:(parse_and_render "[[imgs/photo.png]]"));

  test "image link with desc" (fun () ->
      assert_eq
        ~expected:"<p><img src=\"imgs/photo.png\" alt=\"A photo\" /></p>\n"
        ~actual:(parse_and_render "[[imgs/photo.png][A photo]]"));

  test "image link strips file prefix" (fun () ->
      assert_eq
        ~expected:
          "<p><img src=\"imgs/diagram.svg\" alt=\"diagram.svg\" /></p>\n"
        ~actual:(parse_and_render "[[file:imgs/diagram.svg]]"));

  test "non-image link unchanged" (fun () ->
      assert_eq
        ~expected:
          "<p><a href=\"https://example.com\">https://example.com</a></p>\n"
        ~actual:(parse_and_render "[[https://example.com]]"));

  test "languages extracts src block languages" (fun () ->
      let doc =
        Orgcaml.Parser.parse
          "#+BEGIN_SRC python\n\
           print(1)\n\
           #+END_SRC\n\n\
           #+BEGIN_SRC ocaml\n\
           let x = 1\n\
           #+END_SRC\n\n\
           #+BEGIN_SRC\n\
           plain\n\
           #+END_SRC"
      in
      let langs = Orgcaml.Parser.languages doc in
      assert_eq ~expected:"2" ~actual:(string_of_int (List.length langs));
      assert_eq ~expected:"python" ~actual:(List.nth langs 0);
      assert_eq ~expected:"ocaml" ~actual:(List.nth langs 1));

  test "comment lines are ignored in HTML" (fun () ->
      assert_eq ~expected:"<h1>Title</h1>\n"
        ~actual:(parse_and_render "# this is a comment\n* Title"));

  test "comment-only document produces no output" (fun () ->
      assert_eq ~expected:"" ~actual:(parse_and_render "# just a comment"));

  test "comment is parsed into AST" (fun () ->
      let doc = Orgcaml.Parser.parse "# hello world" in
      match doc.content with
      | [ Orgcaml.Ast.Comment text ] ->
          assert_eq ~expected:"hello world" ~actual:text
      | _ -> failwith "expected a single Comment node");

  test "bare hash is a comment" (fun () ->
      assert_eq ~expected:"" ~actual:(parse_and_render "#"));

  test "full document" (fun () ->
      let input =
        String.concat "\n"
          [
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
          ]
      in
      let html = parse_and_render input in
      assert (String.length html > 0);
      assert (String.sub html 0 4 = "<h1>");
      print_endline "(content OK)");

  print_endline "\nAll tests complete."
