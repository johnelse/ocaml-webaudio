open Js_of_ocaml
open Test_common
open Webtest.Suite

let test_createBuffer () =
  with_context_sync
    (fun context ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in

      assert_equal buffer##.numberOfChannels 1;
      assert_equal buffer##.length buffer_length;
      assert_equal buffer##.sampleRate sample_rate)

let test_buffer_getChannelData () =
  with_context_sync
    (fun context ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in

      let channel0Data = buffer##getChannelData 0 in
      assert_equal channel0Data##.length buffer_length;
      assert_raises_string
        "IndexSizeError: AudioBuffer.getChannelData: Channel number (1) is out of range"
        (fun () ->
          let (_:Typed_array.float32Array Js.t) = buffer##getChannelData 1 in
          ()))

let test_buffer_copyFromChannel () =
  with_context_sync
    (fun context ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in
      let dst = new%js Typed_array.float32Array buffer_length in
      buffer##copyFromChannel dst 0 0)

let test_buffer_copyToChannel () =
  with_context_sync
    (fun context ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in
      let src = new%js Typed_array.float32Array buffer_length in
      buffer##copyToChannel src 0 0)

let test_buffer_copy_both_ways () =
  with_context_sync
    (fun context ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in
      let src = new%js Typed_array.float32Array buffer_length in
      let dst = new%js Typed_array.float32Array buffer_length in
      fill_with_sine_wave src;
      buffer##copyToChannel src 0 0;
      buffer##copyFromChannel dst 0 0;
      for i = 0 to buffer_length - 1 do
        assert_equal (Typed_array.get src i) (Typed_array.get dst i)
      done)

let test_createBufferSource () =
  with_context_sync
    (fun context ->
      let bufferSource = context##createBufferSource in

      assert_equal bufferSource##.numberOfInputs 0;
      assert_equal bufferSource##.numberOfOutputs 1;
      assert_equal bufferSource##.channelCountMode (Js.string "max");
      assert_equal bufferSource##.channelInterpretation (Js.string "speakers");

      bufferSource##.detune##.value := 100.0;
      assert_equal bufferSource##.detune##.value 100.0;
      bufferSource##.loop := Js._true;
      assert_equal bufferSource##.loop Js._true;
      bufferSource##.loopStart := 0.1;
      assert_equal bufferSource##.loopStart 0.1;
      bufferSource##.loopEnd := 0.9;
      assert_equal bufferSource##.loopEnd 0.9;
      bufferSource##.playbackRate##.value := 0.5;
      assert_equal bufferSource##.playbackRate##.value 0.5)

let test_play_bufferSource () =
  with_context_sync
    (fun context ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in
      let src = new%js Typed_array.float32Array buffer_length in
      fill_with_sine_wave src;
      buffer##copyToChannel src 0 0;

      let bufferSource = context##createBufferSource in
      bufferSource##.buffer := buffer;

      bufferSource##connect (context##.destination :> WebAudio.audioNode Js.t);
      bufferSource##start;
      bufferSource##stop)

let test_bufferSource_onended =
  with_context_async
    (fun context wrapper ->
      let buffer = context##createBuffer 1 buffer_length sample_rate in
      let src = new%js Typed_array.float32Array buffer_length in
      fill_with_sine_wave src;
      buffer##copyToChannel src 0 0;

      let bufferSource = context##createBufferSource in
      bufferSource##.buffer := buffer;

      bufferSource##.loopStart := 0.0;
      bufferSource##.loopEnd := 0.1;

      bufferSource##.onended :=
        Dom.handler (fun _ -> wrapper Async.noop; Js._false);

      bufferSource##connect (context##.destination :> WebAudio.audioNode Js.t);
      bufferSource##start)

let with_audioBuffer context uri fn =
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string uri) Js._true;
  request##send Js.null;
  Js.Opt.iter
    (File.CoerceTo.arrayBuffer (request##.response))
    (fun arrayBuffer -> context##decodeAudioData arrayBuffer fn)

let test_decodeAudioData =
  with_context_async
    (fun context wrapper ->
      with_audioBuffer context "data/sound.ogg"
        (fun buffer ->
          let bufferSource = context##createBufferSource in
          bufferSource##.buffer := buffer;

          bufferSource##.onended :=
            Dom.handler (fun _ -> wrapper Async.noop; Js._false);

          bufferSource##connect
            (context##.destination :> WebAudio.audioNode Js.t);
          bufferSource##start))

let suite =
  "buffer" >::: [
    "test_createBuffer" >:: test_createBuffer;
    "test_buffer_getChannelData" >:: test_buffer_getChannelData;
    "test_buffer_copyFromChannel" >:: test_buffer_copyFromChannel;
    "test_buffer_copyToChannel" >:: test_buffer_copyToChannel;
    "test_buffer_copy_both_ways" >:: test_buffer_copy_both_ways;
    "test_createBufferSource" >:: test_createBufferSource;
    "test_play_bufferSource" >:: test_play_bufferSource;
    "test_bufferSource_onended" >:~ test_bufferSource_onended;
  ]
