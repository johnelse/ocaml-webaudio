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

let test_create_context () =
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

let test_create_oscillator () =
  with_context_sync
    (fun context ->
      let oscillator = context##createOscillator in

      assert_equal (oscillator##.numberOfInputs) 0;
      assert_equal (oscillator##.numberOfOutputs) 1;
      assert_equal (oscillator##.channelCountMode) (Js.string "max");
      assert_equal (oscillator##.channelCount) 2;
      assert_equal
        (oscillator##.channelInterpretation) (Js.string "speakers");

      oscillator##.detune##.value := 100.0;
      assert_equal (oscillator##.detune##.value) 100.0;
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

let test_create_biquadFilter () =
  with_context_sync
    (fun context ->
      let biquadFilter = context##createBiquadFilter in

      assert_equal (biquadFilter##.numberOfInputs) 1;
      assert_equal (biquadFilter##.numberOfOutputs) 1;
      assert_equal (biquadFilter##.channelCountMode) (Js.string "max");
      assert_equal (biquadFilter##.channelCount) 2;
      assert_equal
        (biquadFilter##.channelInterpretation) (Js.string "speakers");

      biquadFilter##.detune##.value := 100.0;
      assert_equal (biquadFilter##.detune##.value) 100.0;
      biquadFilter##.frequency##.value := 200.0;
      assert_equal (biquadFilter##.frequency##.value) 200.0;
      biquadFilter##._type := (Js.string "lowpass");
      assert_equal (biquadFilter ##._type) (Js.string "lowpass");
      biquadFilter##._Q##.value := 2.0;
      assert_equal (biquadFilter##._Q##.value) 2.0;
      assert_equal (biquadFilter##.gain##.value) 0.0;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect ((biquadFilter :> WebAudio.audioNode Js.t));
      biquadFilter##connect ((context##.destination :> WebAudio.audioNode Js.t));
      oscillator##start;
      oscillator##stop)

let test_set_biquadFilter_type () =
  with_context_sync
    (fun context ->
      let biquadFilter = context##createBiquadFilter in

      let allowed_types = [
        "lowpass";
        "highpass";
        "bandpass";
        "lowshelf";
        "highshelf";
        "peaking";
        "notch";
        "allpass";
      ] in

      List.iter
        (fun allowed_type ->
          biquadFilter##._type := (Js.string allowed_type);
          assert_equal (biquadFilter##._type) (Js.string allowed_type))
        allowed_types)

let suite =
  "base_suite" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_create_context" >:: test_create_context;
    "test_suspend_resume" >:: test_suspend_resume;
    "test_destination" >:: test_destination;
    "test_create_oscillator" >:: test_create_oscillator;
    "test_set_oscillator_type" >:: test_set_oscillator_type;
    "test_oscillator_onended" >:~ test_oscillator_onended;
    "test_create_biquadFilter" >:: test_create_biquadFilter;
    "test_set_biquadFilter_type" >:: test_set_biquadFilter_type;
  ]
