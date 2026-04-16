(** Equivalence tests: evaluate (streaming) vs evaluate_on_dom (DOM walker).

    Each test parses the same XML both ways and verifies the match_result
    lists are identical. *)

open Alsdiff_base

(* --- XML string constants (same as test_upath2.ml) --- *)

let sample_xml_str =
  "<root>\
   <a id=\"1\">\
   <b>hello</b>\
   <c val=\"test\"/>\
   </a>\
   <a id=\"2\">\
   <d/>\
   <b lang=\"en\">world</b>\
   </a>\
   <e>\
   <child id=\"e-child\"/>\
   <f>\
   <b/>\
   </f>\
   </e>\
   <special type=\"magic\">\
   <child id=\"s-child\"/>\
   </special>\
   <child type=\"magic\"/>\
   </root>"

let nested_xml_str =
  "<root>\
   <a id=\"1\">\
   <b>\
   <c v=\"1\"/>\
   <c v=\"2\"/>\
   </b>\
   </a>\
   <a id=\"2\">\
   <d>\
   <b>\
   <c v=\"3\"/>\
   </b>\
   </d>\
   </a>\
   </root>"

let complex_xml_str =
  "<archive>\
   <library name=\"music\">\
   <section id=\"A\">\
   <artist name=\"Artist1\">\
   <album title=\"Album1.1\" year=\"2020\">\
   <track no=\"1\">Track1</track>\
   <track no=\"2\" feat=\"Artist2\">Track2</track>\
   </album>\
   <album title=\"Album1.2\" year=\"2022\">\
   <track no=\"1\">TrackA</track>\
   </album>\
   </artist>\
   </section>\
   <section id=\"B\">\
   <artist name=\"Artist2\">\
   <album title=\"Album2.1\" year=\"2021\">\
   <track no=\"1\">Single</track>\
   </album>\
   </artist>\
   <genre name=\"electronic\">\
   <artist name=\"Artist3\">\
   <album title=\"Album3.1\" year=\"2023\">\
   <track no=\"1\">E-Track1</track>\
   <track no=\"2\">E-Track2</track>\
   </album>\
   </artist>\
   </genre>\
   </section>\
   </library>\
   <library name=\"pictures\">\
   <album title=\"Holidays\" year=\"2022\">\
   <photo year=\"2022\" location=\"Beach\"/>\
   <photo year=\"2023\" location=\"Mountain\"/>\
   </album>\
   </library>\
   </archive>"

let wildcard_xml_str =
  "<root>\
   <parent1>\
   <child>direct_child</child>\
   </parent1>\
   <parent2>\
   <intermediate>\
   <child>deep_child</child>\
   </intermediate>\
   </parent2>\
   </root>"

(* --- Helpers --- *)

type results = Upath2.match_result list

let eval_streaming path_str xml_str : results =
  let q = Upath2.query_of_path path_str in
  let nfa = Upath2.compile [ q ] in
  let stream = Xml2.stream_from_string xml_str in
  Upath2.evaluate nfa stream

let eval_dom path_str xml_str : results =
  let q = Upath2.query_of_path path_str in
  let nfa = Upath2.compile [ q ] in
  let xml = Xml.read_string xml_str in
  Upath2.evaluate_on_dom nfa xml

let eval_multi_streaming paths xml_str : results =
  let queries = List.map Upath2.query_of_path paths in
  let nfa = Upath2.compile queries in
  let stream = Xml2.stream_from_string xml_str in
  Upath2.evaluate nfa stream

let eval_multi_dom paths xml_str : results =
  let queries = List.map Upath2.query_of_path paths in
  let nfa = Upath2.compile queries in
  let xml = Xml.read_string xml_str in
  Upath2.evaluate_on_dom nfa xml

(* Compare two result lists field-by-field.
   text_content is mutable so we compare via structural equality. *)
let results_equal (a : results) (b : results) =
  List.length a = List.length b
  && List.for_all2 (fun (r1 : Upath2.match_result) (r2 : Upath2.match_result) ->
      r1.query_id = r2.query_id
      && r1.element_name = r2.element_name
      && r1.attrs = r2.attrs
      && r1.depth = r2.depth
      && r1.text_content = r2.text_content
    ) a b

let check_equiv label path_str xml_str =
  let streaming = eval_streaming path_str xml_str in
  let dom = eval_dom path_str xml_str in
  Alcotest.(check bool) (label ^ " equivalence") true (results_equal streaming dom)

let check_multi_equiv label paths xml_str =
  let streaming = eval_multi_streaming paths xml_str in
  let dom = eval_multi_dom paths xml_str in
  Alcotest.(check bool) (label ^ " equivalence") true (results_equal streaming dom)

(* --- Tests --- *)

let simple_tests =
  let module T = struct
    let exact_path () =
      check_equiv "exact path" "/root/a/b" sample_xml_str

    let attr_extraction () =
      check_equiv "attr extraction" "/root/a/c@val" sample_xml_str

    let index_query () =
      check_equiv "index [0]" "/root/a[0]/b" sample_xml_str

    let index_query_1 () =
      check_equiv "index [1]" "/root/a[1]/b" sample_xml_str

    let attr_constraint () =
      check_equiv "attr constraint" "/root/a@id=\"2\"/d" sample_xml_str

    let no_match () =
      check_equiv "no match" "/root/a/nonexistent" sample_xml_str

    let index_out_of_bounds () =
      check_equiv "index out of bounds" "/root/a[2]/b" sample_xml_str

    let single_wildcard () =
      check_equiv "single wildcard" "/root/*/b" sample_xml_str

    let multi_wildcard () =
      check_equiv "multi wildcard" "/root/**/b" sample_xml_str

    let deep_multi_wildcard () =
      check_equiv "deep multi wildcard" "/**/f/b" sample_xml_str

    let root_path () =
      check_equiv "root element" "/root" sample_xml_str

    let text_content () =
      check_equiv "text content" "/root/a/b" sample_xml_str;
      let streaming = eval_streaming "/root/a/b" sample_xml_str in
      let dom = eval_dom "/root/a/b" sample_xml_str in
      let texts s = List.filter_map (fun (r : Upath2.match_result) ->
          r.text_content) s in
      Alcotest.(check bool) "text content values" true
        (texts streaming = texts dom)
  end in
  [
    ("exact path", `Quick, T.exact_path);
    ("attr extraction", `Quick, T.attr_extraction);
    ("index [0]", `Quick, T.index_query);
    ("index [1]", `Quick, T.index_query_1);
    ("attr constraint", `Quick, T.attr_constraint);
    ("no match", `Quick, T.no_match);
    ("index out of bounds", `Quick, T.index_out_of_bounds);
    ("single wildcard", `Quick, T.single_wildcard);
    ("multi wildcard", `Quick, T.multi_wildcard);
    ("deep multi wildcard", `Quick, T.deep_multi_wildcard);
    ("root element", `Quick, T.root_path);
    ("text content", `Quick, T.text_content);
  ]

let wildcard_tests =
  let module T = struct
    let single_wc_child () =
      check_equiv "/*/child" "/root/*/child" wildcard_xml_str

    let multi_wc_child () =
      check_equiv "/**/child" "/root/**/child" wildcard_xml_str

    let deep_multi_wc () =
      check_equiv "deep **" "/**/child" wildcard_xml_str

    let wc_with_attr () =
      check_equiv "wildcard + attr" "/**@id" sample_xml_str

    let wc_with_attr_value () =
      check_equiv "wildcard + attr value" "/**@id=\"2\"" sample_xml_str

    let wc_attr_then_child () =
      check_equiv "wildcard attr then child" "/**@type/child" sample_xml_str

    let star_attr () =
      check_equiv "star with attr" "/*@id" sample_xml_str

    let star_attr_value () =
      check_equiv "star attr value" "/*@type=\"magic\"" sample_xml_str
  end in
  [
    ("/*/child", `Quick, T.single_wc_child);
    ("/**/child", `Quick, T.multi_wc_child);
    ("deep **", `Quick, T.deep_multi_wc);
    ("wildcard + attr", `Quick, T.wc_with_attr);
    ("wildcard + attr value", `Quick, T.wc_with_attr_value);
    ("wildcard attr then child", `Quick, T.wc_attr_then_child);
    ("star with attr", `Quick, T.star_attr);
    ("star attr value", `Quick, T.star_attr_value);
  ]

let nested_tests =
  let module T = struct
    let simple_nested () =
      check_equiv "simple nested" "/root/a/b" nested_xml_str

    let deep_nested () =
      check_equiv "deep nested" "/root/a/d/b" nested_xml_str

    let multi_wc_nested () =
      check_equiv "multi wildcard nested" "/root/**/b" nested_xml_str

    let index_nested () =
      check_equiv "index nested" "/root/a/b/c[1]" nested_xml_str

    let deep_index () =
      check_equiv "deep index" "/root/a[1]/d/b" nested_xml_str

    let all_c () =
      check_equiv "all c" "/root/**/c" nested_xml_str
  end in
  [
    ("simple nested", `Quick, T.simple_nested);
    ("deep nested", `Quick, T.deep_nested);
    ("multi wildcard nested", `Quick, T.multi_wc_nested);
    ("index nested", `Quick, T.index_nested);
    ("deep index", `Quick, T.deep_index);
    ("all c", `Quick, T.all_c);
  ]

let complex_tests =
  let module T = struct
    let deep_attrs () =
      check_equiv "deep path with attrs"
        "/archive/library@name=\"music\"/section@id=\"A\"/artist/album@year=\"2022\"/track"
        complex_xml_str

    let multi_wc_attr () =
      check_equiv "multiwildcard + attr"
        "/**/album@year=\"2021\"/track" complex_xml_str

    let index_wc () =
      check_equiv "index + wildcard"
        "/archive/library[1]/*/photo[0]" complex_xml_str

    let track_feat () =
      check_equiv "track@feat"
        "/**/track@feat=\"Artist2\"" complex_xml_str

    let all_tracks () =
      check_equiv "/**/track" "/**/track" complex_xml_str

    let multi_index () =
      check_equiv "multi-index"
        "/archive/library@name=\"music\"/section[0]/artist@name=\"Artist1\"/album[1]/track[0]"
        complex_xml_str

    let regex_name () =
      check_equiv "regex name" "/root/'(a|e)'" sample_xml_str
  end in
  [
    ("deep attrs", `Quick, T.deep_attrs);
    ("multiwildcard + attr", `Quick, T.multi_wc_attr);
    ("index + wildcard", `Quick, T.index_wc);
    ("track@feat", `Quick, T.track_feat);
    ("/**/track", `Quick, T.all_tracks);
    ("multi-index", `Quick, T.multi_index);
    ("regex name", `Quick, T.regex_name);
  ]

let multi_query_tests =
  let module T = struct
    let two_queries () =
      check_multi_equiv "two queries"
        [ "/root/a@id"; "/root/a/b" ] sample_xml_str

    let three_queries () =
      check_multi_equiv "three queries"
        [ "/root/a/b"; "/root/a/c@val"; "/root/a/d" ] sample_xml_str

    let multi_wc_queries () =
      check_multi_equiv "multi wildcard queries"
        [ "/**/b"; "/**/c"; "/**/child" ] sample_xml_str

    let nested_multi () =
      check_multi_equiv "nested multi"
        [ "/root/a/b/c"; "/root/a[0]/**/c" ] nested_xml_str
  end in
  [
    ("two queries", `Quick, T.two_queries);
    ("three queries", `Quick, T.three_queries);
    ("multi wildcard queries", `Quick, T.multi_wc_queries);
    ("nested multi", `Quick, T.nested_multi);
  ]

let () =
  Alcotest.run "Upath2 DOM Equivalence" [
    "simple", List.map (fun (n, s, f) -> Alcotest.test_case n s f)
      simple_tests;
    "wildcard", List.map (fun (n, s, f) -> Alcotest.test_case n s f)
      wildcard_tests;
    "nested", List.map (fun (n, s, f) -> Alcotest.test_case n s f)
      nested_tests;
    "complex", List.map (fun (n, s, f) -> Alcotest.test_case n s f)
      complex_tests;
    "multi_query", List.map (fun (n, s, f) -> Alcotest.test_case n s f)
      multi_query_tests;
  ]
