open Js_of_ocaml
open Test_common
open Webtest.Suite

let with_offline_context_sync channels length sample_rate f =
  Sync.bracket
    (fun () -> new%js WebAudio.offlineAudioContext channels length sample_rate)
    f
    (fun _ -> ())
    ()

let with_offline_context_async channels length sample_rate f =
  Async.bracket
    (fun () -> new%js WebAudio.offlineAudioContext channels length sample_rate)
    f
    (fun _ -> ())

let test_create_offlineAudioContext () =
  with_offline_context_sync 1 buffer_length sample_rate
    (fun context ->
      assert_equal context##.length buffer_length;
      assert_equal context##.sampleRate sample_rate;
      ())

let test_offline_render =
  with_offline_context_async 1 buffer_length sample_rate
    (fun context wrapper ->
      let src = new%js Typed_array.float32Array buffer_length in
      let dst = new%js Typed_array.float32Array buffer_length in
      fill_with_sine_wave src;
      let buffer = context##createBuffer 1 buffer_length sample_rate in
      buffer##copyToChannel src 0 0;

      let bufferSource = context##createBufferSource in
      bufferSource##.buffer := buffer;

      bufferSource##connect (context##.destination :> WebAudio.audioNode Js.t);
      bufferSource##start;

      context##.oncomplete :=
        Dom.handler (fun completionEvent ->
          wrapper (fun () ->
            completionEvent##.renderedBuffer##copyFromChannel dst 0 0;

            for i = 0 to buffer_length - 1 do
              assert_equal (Typed_array.get src i) (Typed_array.get dst i)
            done);

          Js._false);

      context##startRendering;)

let suite =
  "offline" >::: [
    "test_create_OfflineAudioContext" >:: test_create_offlineAudioContext;
    "test_offline_render" >:~ test_offline_render;
  ]
