module T = Test_utils

let (>::) = T.(>::)
let (>:::) = T.(>:::)

let with_context f =
  let context = jsnew WebAudio.audioContext () in
  T.finally (fun () -> f context) (fun () -> context##close ())

let test_make_context () =
  with_context (fun _ -> ())

let test_make_oscillator () =
  with_context
    (fun context ->
      let oscillator = context##createOscillator () in
      oscillator##frequency##value <- 200.0;
      T.assert_equal (oscillator##frequency##value) 200.0;
      oscillator##_type <- (Js.string "sine");
      T.assert_equal (oscillator##_type) (Js.string "sine");
      oscillator##connect_AudioNode (context##destination);
      oscillator##start ();
      oscillator##stop ()
    )

let suite =
  "base_suite" >::: [
    "test_make_context" >:: test_make_context;
    "test_make_oscillator" >:: test_make_oscillator;
  ]

let run_suite log =
  let open T in
  let (_ : result list) = run log suite in
  Js._false
