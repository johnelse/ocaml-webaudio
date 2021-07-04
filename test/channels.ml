open Js_of_ocaml
open Test_common
open Webtest.Suite

let test_createChannelMerger () =
  with_context_sync
    (fun context ->
      let merger = context##createChannelMerger 4 in

      assert_equal merger##.numberOfInputs 4;
      assert_equal merger##.numberOfOutputs 1;
      assert_equal merger##.channelCountMode (Js.string "explicit");
      assert_equal merger##.channelCount 1;
      assert_equal merger##.channelInterpretation (Js.string "speakers"))

let test_createChannelSplitter () =
  with_context_sync
    (fun context ->
      let splitter = context##createChannelSplitter 4 in

      assert_equal splitter##.numberOfInputs 1;
      assert_equal splitter##.numberOfOutputs 4;
      assert_equal splitter##.channelCountMode (Js.string "explicit");
      assert_equal splitter##.channelCount 4;
      assert_equal splitter##.channelInterpretation (Js.string "discrete"))

let test_create_splitter_merger () =
  with_context_sync
    (fun context ->
      let splitter = context##createChannelSplitter 2 in
      let merger = context##createChannelMerger 2 in

      let oscillator = context##createOscillator in
      oscillator##.frequency##.value := 200.0;
      oscillator##._type := (Js.string "square");

      oscillator##connect (splitter :> WebAudio.audioNode Js.t);
      splitter##connect_outputToInput (merger :> WebAudio.audioNode Js.t) 0 1;
      splitter##connect_outputToInput (merger :> WebAudio.audioNode Js.t) 1 0;

      oscillator##start;
      oscillator##stop)

let suite =
  "channels" >::: [
    "test_createChannelMerger" >:: test_createChannelMerger;
    "test_createChannelSplitter" >:: test_createChannelSplitter;
    "test_create_splitter_merger" >:: test_create_splitter_merger;
  ]
