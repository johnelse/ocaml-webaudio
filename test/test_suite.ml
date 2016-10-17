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

let with_offline_context_sync channels length sample_rate f =
  Sync.bracket
    (fun () -> new%js WebAudio.offlineAudioContext channels length sample_rate)
    f
    (fun context -> context##close)
    ()

let with_offline_context_async channels length sample_rate f =
  Async.bracket
    (fun () -> new%js WebAudio.offlineAudioContext channels length sample_rate)
    f
    (fun context -> context##close)

let test_create_context () =
  with_context_sync (fun _ -> ())

let test_context_fields () =
  with_context_sync
    (fun context ->
      (* There's no guaranteeing what the value of these fields will be, but
         let's make sure they can be read. *)
      let (_:float) = context##.currentTime in
      let (_:float) = context##.sampleRate in
      let (_:Js.js_string Js.t) = context##.state in
      ())

let test_suspend_resume () =
  with_context_sync
    (fun context ->
      context##suspend;
      context##resume)

let test_context_onstatechange =
  with_context_async
    (fun context wrapper ->
      (* onstatechange will be raised twice - once when the context's state is
         set to "running" and once when its state is set to "closed". Webtest
         will handle the first one to be called. *)
      context##.onstatechange :=
        Dom.handler (fun _ ->
          wrapper Async.noop;
          Js._false))

let test_destination () =
  with_context_sync
    (fun context ->
      let destination = context##.destination in
      assert_equal destination##.numberOfInputs 1;
      assert_equal destination##.numberOfOutputs 0;

      assert_equal destination##.channelCountMode (Js.string "explicit");
      assert_equal destination##.channelCount 2;
      assert_equal
        (destination##.channelInterpretation) (Js.string "speakers");
      (* TODO: Figure out why snd-dummy causes Firefox to report
               maxChannelCount as 10000! *)
      assert_true
        "maxChannelCount should be an expected number"
        (List.mem destination##.maxChannelCount [2; 10000]))

let environment =
  "environment" >::: [
    "test_is_supported" >:: test_is_supported;
    "test_create_context" >:: test_create_context;
    "test_context_fields" >:: test_context_fields;
    "test_suspend_resume" >:: test_suspend_resume;
    "test_context_onstatechange" >:~ test_context_onstatechange;
    "test_destination" >:: test_destination;
  ]

let buffer_length = 44100
let sample_rate = 44100.0
let pi = 2.0 *. (asin 1.0)

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
        "SyntaxError: An invalid or illegal string was specified"
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

let fill_with_sine_wave array =
  (* Fill buffer with a second of 440Hz sine wave. *)
  let frequency = 440.0 in
  let samples_per_period = sample_rate /. frequency in
  for i = 0 to array##.length - 1 do
    let amplitude =
      sin (2.0 *. pi *. (float_of_int i) /. samples_per_period) in
    Typed_array.set array i amplitude
  done

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
  Lwt.bind
    XmlHttpRequest.(
      perform_raw ~response_type:ArrayBuffer "data/sound.ogg")
    (fun response ->
      Lwt.return (Js.Opt.iter
        response.XmlHttpRequest.content
        (fun arrayBuffer -> context##decodeAudioData arrayBuffer fn)))

let test_decodeAudioData =
  with_context_async
    (fun context wrapper ->
      Lwt_js_events.async
        (fun () ->
          (with_audioBuffer context "data/soung.ogg"
            (fun buffer ->
              let bufferSource = context##createBufferSource in
              bufferSource##.buffer := buffer;

              bufferSource##.onended :=
                Dom.handler (fun _ -> wrapper Async.noop; Js._false);

              bufferSource##connect
                (context##.destination :> WebAudio.audioNode Js.t);
              bufferSource##start))))

let buffer =
  "buffer" >::: [
    "test_createBuffer" >:: test_createBuffer;
    "test_buffer_getChannelData" >:: test_buffer_getChannelData;
    "test_buffer_copyFromChannel" >:: test_buffer_copyFromChannel;
    "test_buffer_copyToChannel" >:: test_buffer_copyToChannel;
    "test_buffer_copy_both_ways" >:: test_buffer_copy_both_ways;
    "test_createBufferSource" >:: test_createBufferSource;
    "test_play_bufferSource" >:: test_play_bufferSource;
    "test_bufferSource_onended" >:~ test_bufferSource_onended;
    "test_decodeAudioData" >:~ test_decodeAudioData;
  ]

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

let oscillator =
  "oscillator" >::: [
    "test_createOscillator" >:: test_createOscillator;
    "test_set_oscillator_type" >:: test_set_oscillator_type;
    "test_oscillator_onended" >:~ test_oscillator_onended;
  ]

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
      assert_equal splitter##.channelCountMode (Js.string "max");
      assert_equal splitter##.channelCount 2;
      assert_equal splitter##.channelInterpretation (Js.string "speakers"))

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

let channels =
  "channels" >::: [
    "test_createChannelMerger" >:: test_createChannelMerger;
    "test_createChannelSplitter" >:: test_createChannelSplitter;
    "test_create_splitter_merger" >:: test_create_splitter_merger;
  ]

let test_createAnalyser () =
  with_context_sync
    (fun context ->
      let analyser = context##createAnalyser in

      assert_equal analyser##.numberOfInputs 1;
      assert_equal analyser##.numberOfOutputs 1;
      assert_equal analyser##.channelCountMode (Js.string "max");
      assert_equal analyser##.channelCount 1;
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
      let buffer = context##createBuffer 2 buffer_length sample_rate in
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
      assert_equal compressor##.channelCountMode (Js.string "explicit");
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

let nodes =
  "nodes" >::: [
    "test_createAnalyser" >:: test_createAnalyser;
    "test_createBiquadFilter" >:: test_createBiquadFilter;
    "test_set_biquadFilter_type" >:: test_set_biquadFilter_type;
    "test_createConvolver" >:: test_createConvolver;
    "test_createDelay" >:: test_createDelay;
    "test_createDynamicsCompressor" >:: test_createDynamicsCompressor;
    "test_createGain" >:: test_createGain;
    "test_createMediaElementSource" >:: test_createMediaElementSource;
    "test_createStereoPanner" >:: test_createStereoPanner;
    "test_createWaveShaper" >:: test_createWaveShaper;
    "test_createPeriodicWave" >:: test_createPeriodicWave;
  ]

let test_create_offlineAudioContext () =
  with_offline_context_sync 1 buffer_length sample_rate
    (fun context ->
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

let offline =
  "offline" >::: [
    "test_create_OfflineAudioContext" >:: test_create_offlineAudioContext;
    "test_offline_render" >:~ test_offline_render;
  ]

let suite =
  "base_suite" >::: [
    environment;
    buffer;
    oscillator;
    channels;
    nodes;
    offline;
  ]
