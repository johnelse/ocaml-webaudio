open Js_of_ocaml
open Test_common
open Webtest.Suite

let test_is_supported () =
  assert_equal (WebAudio.is_supported()) true

let test_create_context () =
  with_context_sync (fun _ -> ())

let test_context_fields () =
  with_context_sync
    (fun context ->
      (* There's no guaranteeing what the value of these fields will be, but
         let's make sure they can be read. *)
      let (_:float) = context##.currentTime in
      let (_:float) = context##.sampleRate in
      let (_:Js.js_string Js.t) = context##.state in
      ())

let test_suspend_resume () =
  with_context_sync
    (fun context ->
      context##suspend;
      context##resume)

let test_context_onstatechange =
  with_context_async
    (fun context wrapper ->
      (* onstatechange will be raised twice - once when the context's state is
         set to "running" and once when its state is set to "closed". Webtest
         will handle the first one to be called. *)
      context##.onstatechange :=
        Dom.handler (fun _ ->
          wrapper Async.noop;
          Js._false))

let test_destination () =
  with_context_sync
    (fun context ->
      let destination = context##.destination in
      assert_equal destination##.numberOfInputs 1;
      assert_equal destination##.numberOfOutputs 0;

      assert_equal destination##.channelCountMode (Js.string "explicit");
      assert_equal destination##.channelCount 2;
      assert_equal
        (destination##.channelInterpretation) (Js.string "speakers");
      (* maxChannelCount is reported as 0 when running under github actions. *)
      assert_true
        ~label:"maxChannelCount should be an expected number"
        (List.mem destination##.maxChannelCount [0; 2]))

let suite =
  "environment" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_create_context" >:: test_create_context;
    "test_context_fields" >:: test_context_fields;
    "test_suspend_resume" >:: test_suspend_resume;
    "test_context_onstatechange" >:~ test_context_onstatechange;
    "test_destination" >:: test_destination;
  ]
