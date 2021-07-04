open Js_of_ocaml
open Test_common
open Webtest.Suite

let test_createOscillator () =
  with_context_sync
    (fun context ->
      let oscillator = context##createOscillator in

      assert_equal oscillator##.numberOfInputs 0;
      assert_equal oscillator##.numberOfOutputs 1;
      assert_equal oscillator##.channelCountMode (Js.string "max");
      assert_equal oscillator##.channelCount 2;
      assert_equal oscillator##.channelInterpretation (Js.string "speakers");

      oscillator##.detune##.value := 100.0;
      assert_equal oscillator##.detune##.value 100.0;
      oscillator##.frequency##.value := 200.0;
      assert_equal oscillator##.frequency##.value 200.0;
      oscillator##._type := (Js.string "sine");
      assert_equal oscillator##._type (Js.string "sine");
      oscillator##connect (context##.destination :> WebAudio.audioNode Js.t);
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
          assert_equal oscillator##._type (Js.string allowed_type))
        allowed_types)

let test_oscillator_onended =
  with_context_async
    (fun context wrapper ->
      let oscillator = context##createOscillator in

      oscillator##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;

      oscillator##.onended :=
        Dom.handler (fun _ -> wrapper Async.noop; Js._false);

      oscillator##stop)

let suite =
  "oscillator" >::: [
    "test_createOscillator" >:: test_createOscillator;
    "test_set_oscillator_type" >:: test_set_oscillator_type;
    "test_oscillator_onended" >:~ test_oscillator_onended;
  ]
