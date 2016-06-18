open Webtest

let finally f cleanup =
  let result =
    try f ()
    with e ->
      cleanup ();
      raise e
  in
  cleanup ();
  result

let test_is_supported () =
  assert_equal (WebAudio.is_supported()) true

let with_context f =
  let context = jsnew WebAudio.audioContext () in
  finally (fun () -> f context) (fun () -> context##close ())

let test_make_context () =
  with_context (fun _ -> ())

let test_suspend_resume () =
  with_context
    (fun context ->
      context##suspend ();
      context##resume ())

let test_destination () =
  with_context
    (fun context ->
      let destination = context##destination in
      assert_equal (destination##numberOfInputs) 1;
      assert_equal (destination##numberOfOutputs) 0;

      assert_equal (destination##channelCountMode) (Js.string "explicit");
      assert_equal (destination##channelCount) 2;
      assert_equal
        (destination##channelInterpretation) (Js.string "speakers");
      assert_equal
        (destination##maxChannelCount) 2)

let test_make_oscillator () =
  with_context
    (fun context ->
      let oscillator = context##createOscillator () in

      assert_equal (oscillator##numberOfInputs) 0;
      assert_equal (oscillator##numberOfOutputs) 1;
      assert_equal (oscillator##channelCountMode) (Js.string "max");
      assert_equal (oscillator##channelCount) 2;
      assert_equal
        (oscillator##channelInterpretation) (Js.string "speakers");

      oscillator##frequency##value <- 200.0;
      assert_equal (oscillator##frequency##value) 200.0;
      oscillator##_type <- (Js.string "sine");
      assert_equal (oscillator##_type) (Js.string "sine");
      oscillator##connect_AudioNode (Js.Unsafe.coerce (context##destination));
      oscillator##start ();
      oscillator##stop ()
    )

let suite =
  "base_suite" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_make_context" >:: test_make_context;
    "test_suspend_resume" >:: test_suspend_resume;
    "test_destination" >:: test_destination;
    "test_make_oscillator" >:: test_make_oscillator;
  ]
