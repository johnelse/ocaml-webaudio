open Webtest.Suite

let buffer_length = 44100
let sample_rate = 44100.0
let pi = Js_of_ocaml.Js.math##._PI

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

let fill_with_sine_wave array =
  (* Fill buffer with a 440Hz sine wave. *)
  let frequency = 440.0 in
  let samples_per_period = sample_rate /. frequency in
  for i = 0 to array##.length - 1 do
    let amplitude =
      sin (2.0 *. pi *. (float_of_int i) /. samples_per_period) in
    Js_of_ocaml.Typed_array.set array i amplitude
  done
