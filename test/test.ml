module Html = Dom_html

let (>::) = Test_utils.(>::)
let (>:::) = Test_utils.(>:::)

let test_make_context () =
  let context = jsnew WebAudio.audioContext () in
  context##close ()

let suite =
  "base_suite" >::: [
    "test_make_context" >:: test_make_context;
  ]

let run_suite log =
  let open Test_utils in
  let (_ : result list) = run log suite in
  Js._false

let finally f cleanup =
  let result =
    try f ()
    with e ->
      cleanup ();
      raise e
  in
  cleanup ();
  result

let with_button_disabled button f =
  (Js.Unsafe.coerce button)##disabled <- Js._true;
  button##innerHTML <- Js.string "Running...";
  finally f
    (fun () ->
      button##innerHTML <- Js.string "Run";
      (Js.Unsafe.coerce button)##disabled <- Js._false)

let start _ =
  let button = Html.getElementById "run" in
  let info = Html.getElementById "info" in
  let log data = Dom.appendChild info
    (Html.document##createTextNode (Js.string (Printf.sprintf "%s\n" data)))
  in
  button##onclick <- Html.handler
    (fun _ ->
      List.iter
        (fun node -> Dom.removeChild info node)
        (info##childNodes |> Dom.list_of_nodeList);
      with_button_disabled button (fun () -> run_suite log));
  Js._false

let () =
  Html.window##onload <- Html.handler start
