open Js_of_ocaml
open Test_common
open Webtest.Suite

let test_createAnalyser () =
  with_context_sync
    (fun context ->
      let analyser = context##createAnalyser in

      assert_equal analyser##.numberOfInputs 1;
      assert_equal analyser##.numberOfOutputs 1;
      assert_equal analyser##.channelCountMode (Js.string "max");
      assert_equal analyser##.channelCount 2;
      assert_equal analyser##.channelInterpretation (Js.string "speakers");

      let fftSize = 2048 in
      let frequencyBinCount = fftSize / 2 in
      analyser##.fftSize := fftSize;
      assert_equal analyser##.fftSize fftSize;
      assert_equal analyser##.frequencyBinCount frequencyBinCount;

      analyser##.minDecibels := -40.0;
      assert_equal analyser##.minDecibels (-40.0);
      analyser##.maxDecibels := -20.0;
      assert_equal analyser##.maxDecibels (-20.0);
      analyser##.smoothingTimeConstant := 0.5;
      assert_equal analyser##.smoothingTimeConstant 0.5;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (analyser :> WebAudio.audioNode Js.t);
      analyser##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;

      let floatFrequencyData =
        new%js Typed_array.float32Array frequencyBinCount in
      analyser##getFloatFrequencyData floatFrequencyData;

      let byteFrequencyData = new%js Typed_array.uint8Array frequencyBinCount in
      analyser##getByteFrequencyData byteFrequencyData;

      let floatTimeDomainData = new%js Typed_array.float32Array fftSize in
      analyser##getFloatTimeDomainData floatTimeDomainData;

      let byteTimeDomainData = new%js Typed_array.uint8Array fftSize in
      analyser##getByteTimeDomainData byteTimeDomainData;

      oscillator##stop)

let test_createBiquadFilter () =
  with_context_sync
    (fun context ->
      let biquadFilter = context##createBiquadFilter in

      assert_equal biquadFilter##.numberOfInputs 1;
      assert_equal biquadFilter##.numberOfOutputs 1;
      assert_equal biquadFilter##.channelCountMode (Js.string "max");
      assert_equal biquadFilter##.channelCount 2;
      assert_equal biquadFilter##.channelInterpretation (Js.string "speakers");

      biquadFilter##.detune##.value := 100.0;
      assert_equal biquadFilter##.detune##.value 100.0;
      biquadFilter##.frequency##.value := 200.0;
      assert_equal biquadFilter##.frequency##.value 200.0;
      biquadFilter##._type := (Js.string "lowpass");
      assert_equal biquadFilter ##._type (Js.string "lowpass");
      biquadFilter##._Q##.value := 2.0;
      assert_equal biquadFilter##._Q##.value 2.0;
      biquadFilter##.gain##.value := 2.0;
      assert_equal biquadFilter##.gain##.value 2.0;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (biquadFilter :> WebAudio.audioNode Js.t);
      biquadFilter##connect (context##.destination :> WebAudio.audioNode Js.t);
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
          assert_equal biquadFilter##._type (Js.string allowed_type))
        allowed_types)

let test_createConvolver () =
  with_context_sync
    (fun context ->
      let convolver = context##createConvolver in

      assert_equal convolver##.numberOfInputs 1;
      assert_equal convolver##.numberOfOutputs 1;
      assert_equal convolver##.channelCountMode (Js.string "clamped-max");
      assert_equal convolver##.channelCount 2;
      assert_equal convolver##.channelInterpretation (Js.string "speakers");

      convolver##.normalize := Js._true;
      assert_equal convolver##.normalize Js._true;

      let buffer_length = 100 in
      let buffer = context##createBuffer 2 buffer_length context##.sampleRate in
      let data = new%js Typed_array.float32Array buffer_length in
      for i = 0 to 99 do
        Typed_array.set data i (1.0 *. (0.9 ** (float_of_int i)))
      done;

      buffer##copyToChannel data 0 0;
      buffer##copyToChannel data 1 0;
      convolver##.buffer := buffer;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (convolver :> WebAudio.audioNode Js.t);
      convolver##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let test_createDelay () =
  with_context_sync
    (fun context ->
      let delay = context##createDelay 5.0 in

      assert_equal delay##.numberOfInputs 1;
      assert_equal delay##.numberOfOutputs 1;
      assert_equal delay##.channelCountMode (Js.string "max");
      assert_equal delay##.channelCount 2;
      assert_equal delay##.channelInterpretation (Js.string "speakers");

      delay##.delayTime##.value := 2.0;
      assert_equal ~printer:string_of_float delay##.delayTime##.value 2.0;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "sine");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (delay :> WebAudio.audioNode Js.t);
      delay##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let test_createDynamicsCompressor () =
  with_context_sync
    (fun context ->
      let compressor = context##createDynamicsCompressor in

      assert_equal compressor##.numberOfInputs 1;
      assert_equal compressor##.numberOfOutputs 1;
      assert_equal compressor##.channelCountMode (Js.string "clamped-max");
      assert_equal compressor##.channelCount 2;
      assert_equal compressor##.channelInterpretation (Js.string "speakers");

      let (_:float) = compressor##.reduction in

      compressor##.attack##.value := 2.0;
      assert_equal compressor##.attack##.value 2.0;
      compressor##.knee##.value := 20.0;
      assert_equal compressor##.knee##.value 20.0;
      compressor##.ratio##.value := 2.0;
      assert_equal compressor##.ratio##.value 2.0;
      compressor##.release##.value := 2.0;
      assert_equal compressor##.release##.value 2.0;
      compressor##.threshold##.value := 40.0;
      assert_equal compressor##.threshold##.value 40.0;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (compressor :> WebAudio.audioNode Js.t);
      compressor##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let test_createGain () =
  with_context_sync
    (fun context ->
      let gain = context##createGain in

      assert_equal gain##.numberOfInputs 1;
      assert_equal gain##.numberOfOutputs 1;
      assert_equal gain##.channelCountMode (Js.string "max");
      assert_equal gain##.channelCount 2;
      assert_equal gain##.channelInterpretation (Js.string "speakers");

      gain##.gain##.value := 2.0;
      assert_equal gain##.gain##.value 2.0;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (gain :> WebAudio.audioNode Js.t);
      gain##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let test_createIIRFilter () =
  with_context_sync
    (fun context ->
      let max_order = 20 in
      let feedforward = new%js Typed_array.float32Array max_order in
      let feedback = new%js Typed_array.float32Array max_order in
      Typed_array.set feedforward 0 1.0;
      Typed_array.set feedback 0 1.0;
      Typed_array.set feedback 1 0.5;
      let filter = context##createIIRFilter feedforward feedback in

      assert_equal filter##.numberOfInputs 1;
      assert_equal filter##.numberOfOutputs 1;
      assert_equal filter##.channelCountMode (Js.string "max");
      assert_equal filter##.channelCount 2;
      assert_equal filter##.channelInterpretation (Js.string "speakers");

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "square");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (filter :> WebAudio.audioNode Js.t);
      filter##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let test_createMediaElementSource () =
  with_context_sync
    (fun context ->
      let audioElement = Dom_html.(createAudio document) in
      audioElement##.src := (Js.string "data/sound.ogg");
      let child =
        Dom_html.document##.body##appendChild (audioElement :> Dom.node Js.t) in

      let mediaElementSource = context##createMediaElementSource audioElement in
      assert_equal mediaElementSource##.numberOfInputs 0;
      assert_equal mediaElementSource##.numberOfOutputs 1;
      assert_equal mediaElementSource##.channelCountMode (Js.string "max");
      assert_equal mediaElementSource##.channelCount 2;
      assert_equal
        mediaElementSource##.channelInterpretation (Js.string "speakers");

      mediaElementSource##connect
        (context##.destination :> WebAudio.audioNode Js.t);
      audioElement##play;
      audioElement##pause;

      let (_ : Dom.node Js.t) =
        Dom_html.document##.body##removeChild child in
      ())

let test_createStereoPanner () =
  with_context_sync
    (fun context ->
      let stereoPanner = context##createStereoPanner in

      assert_equal stereoPanner##.numberOfInputs 1;
      assert_equal stereoPanner##.numberOfOutputs 1;
      assert_equal stereoPanner##.channelCountMode (Js.string "clamped-max");
      assert_equal stereoPanner##.channelCount 2;
      assert_equal stereoPanner##.channelInterpretation (Js.string "speakers");

      stereoPanner##.pan##.value := (-0.5);
      assert_equal ~printer:string_of_float stereoPanner##.pan##.value (-0.5);
      stereoPanner##.pan##.value := 0.5;
      assert_equal ~printer:string_of_float stereoPanner##.pan##.value 0.5;

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "sine");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (stereoPanner :> WebAudio.audioNode Js.t);
      stereoPanner##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let make_distortion_curve amount =
  let abs_float x =
    if x >= 0. then x
    else 0.0 -. x
  in
  (* See https://developer.mozilla.org/en-US/docs/Web/API/WaveShaperNode *)
  let amount = max 0.0 amount in
  let samples = int_of_float sample_rate in
  let curve = new%js Typed_array.float32Array samples in
  let deg = pi /. 180.0 in
  for i = 0 to (samples - 1) do
    let x = (float_of_int i) *. 2.0 /. sample_rate -. 1.0 in
    Typed_array.set curve i
      ((3.0 +. amount) *. x *. 20. *. deg /. (pi +. amount *. (abs_float x)))
  done;
  curve

let test_createWaveShaper () =
  with_context_sync
    (fun context ->
      let waveShaper = context##createWaveShaper in

      assert_equal waveShaper##.numberOfInputs 1;
      assert_equal waveShaper##.numberOfOutputs 1;
      assert_equal waveShaper##.channelCountMode (Js.string "max");
      assert_equal waveShaper##.channelCount 2;
      assert_equal waveShaper##.channelInterpretation (Js.string "speakers");

      waveShaper##.oversample := (Js.string "none");
      assert_equal waveShaper##.oversample (Js.string "none");
      waveShaper##.oversample := (Js.string "2x");
      assert_equal waveShaper##.oversample (Js.string "2x");
      waveShaper##.oversample := (Js.string "4x");
      assert_equal waveShaper##.oversample (Js.string "4x");

      waveShaper##.curve := (make_distortion_curve 10.0);

      let oscillator = context##createOscillator in
      oscillator##._type := (Js.string "sine");
      oscillator##.frequency##.value := 200.0;

      oscillator##connect (waveShaper :> WebAudio.audioNode Js.t);
      waveShaper##connect (context##.destination :> WebAudio.audioNode Js.t);
      oscillator##start;
      oscillator##stop)

let test_createPeriodicWave () =
  with_context_sync
    (fun context ->
      let real = new%js Typed_array.float32Array 2 in
      let imag = new%js Typed_array.float32Array 2 in

      Typed_array.set real 0 0.0;
      Typed_array.set imag 0 0.0;
      Typed_array.set real 1 1.0;
      Typed_array.set imag 1 0.0;

      let periodicWave = context##createPeriodicWave real imag in

      let oscillator = context##createOscillator in
      oscillator##setPeriodicWave periodicWave;
      oscillator##connect (context##.destination :> WebAudio.audioNode Js.t);

      oscillator##start;
      oscillator##stop)

let suite =
  "nodes" >::: [
    "test_createAnalyser" >:: test_createAnalyser;
    "test_createBiquadFilter" >:: test_createBiquadFilter;
    "test_set_biquadFilter_type" >:: test_set_biquadFilter_type;
    "test_createConvolver" >:: test_createConvolver;
    "test_createDelay" >:: test_createDelay;
    "test_createDynamicsCompressor" >:: test_createDynamicsCompressor;
    "test_createGain" >:: test_createGain;
    "test_createIIRFilter" >:: test_createIIRFilter;
    "test_createMediaElementSource" >:: test_createMediaElementSource;
    "test_createStereoPanner" >:: test_createStereoPanner;
    "test_createWaveShaper" >:: test_createWaveShaper;
    "test_createPeriodicWave" >:: test_createPeriodicWave;
  ]
