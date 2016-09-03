open Webtest.Suite

let test_is_supported () =
  assert_equal (WebAudio.is_supported()) true

let with_context_sync f =
  Sync.bracket
    (fun () -> new%js WebAudio.audioContext)
    f
    (fun context -> context##close)
    ()

let with_context_async f =
  Async.bracket
    (fun () -> new%js WebAudio.audioContext)
    f
    (fun context -> context##close)

let test_make_context () =
  with_context_sync (fun _ -> ())

let test_suspend_resume () =
  with_context_sync
    (fun context ->
      context##suspend;
      context##resume)

let test_destination () =
  with_context_sync
    (fun context ->
      let destination = context##.destination in
      assert_equal (destination##.numberOfInputs) 1;
      assert_equal (destination##.numberOfOutputs) 0;

      assert_equal (destination##.channelCountMode) (Js.string "explicit");
      assert_equal (destination##.channelCount) 2;
      assert_equal
        (destination##.channelInterpretation) (Js.string "speakers");
      assert_equal
        (destination##.maxChannelCount) 2)

let test_make_oscillator () =
  with_context_sync
    (fun context ->
      let oscillator = context##createOscillator in

      assert_equal (oscillator##.numberOfInputs) 0;
      assert_equal (oscillator##.numberOfOutputs) 1;
      assert_equal (oscillator##.channelCountMode) (Js.string "max");
      assert_equal (oscillator##.channelCount) 2;
      assert_equal
        (oscillator##.channelInterpretation) (Js.string "speakers");

      oscillator##.frequency##.value := 200.0;
      assert_equal (oscillator##.frequency##.value) 200.0;
      oscillator##._type := (Js.string "sine");
      assert_equal (oscillator##._type) (Js.string "sine");
      oscillator##(connect ((context##.destination :> WebAudio.audioNode Js.t)));
      oscillator##start;
      oscillator##stop
    )

let test_set_oscillator_type () =
  with_context_sync
    (fun context ->
      let oscillator = context##createOscillator in

      let allowed_types = [
        "sine";
        "square";
        "sawtooth";
        "triangle";
      ] in

      List.iter
        (fun allowed_type ->
          oscillator##._type := (Js.string allowed_type);
          assert_equal (oscillator##._type) (Js.string allowed_type))
        allowed_types)

let test_oscillator_onended =
  with_context_async
    (fun context callback ->
      let oscillator = context##createOscillator in

      oscillator##connect ((context##.destination :> WebAudio.audioNode Js.t));
      oscillator##start;

      oscillator##.onended :=
        Dom_html.handler (fun _ -> callback (); Js._false);

      oscillator##stop)

let suite =
  "base_suite" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_make_context" >:: test_make_context;
    "test_suspend_resume" >:: test_suspend_resume;
    "test_destination" >:: test_destination;
    "test_make_oscillator" >:: test_make_oscillator;
    "test_set_oscillator_type" >:: test_set_oscillator_type;
    "test_oscillator_onended" >:~ test_oscillator_onended;
  ]
