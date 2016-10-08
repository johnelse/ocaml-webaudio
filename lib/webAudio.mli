class type audioBuffer = object
  method sampleRate : float Js.readonly_prop
  method length : int Js.readonly_prop
  method duration : float Js.readonly_prop
  method numberOfChannels : int Js.readonly_prop

  method getChannelData : int -> Typed_array.float32Array Js.t Js.meth
  method copyFromChannel : Typed_array.float32Array Js.t -> int -> int -> unit Js.meth
  method copyToChannel : Typed_array.float32Array Js.t -> int -> int -> unit Js.meth
end

class type audioNode = object
  method context : audioContext Js.t Js.readonly_prop
  method numberOfInputs : int Js.readonly_prop
  method numberOfOutputs : int Js.readonly_prop

  method channelCount : int Js.prop
  method channelCountMode : Js.js_string Js.t Js.prop
  method channelInterpretation : Js.js_string Js.t Js.prop

  method connect : audioNode Js.t -> unit Js.meth
  method connect_output : audioNode Js.t -> int -> unit Js.meth
  method connect_outputToInput : audioNode Js.t -> int -> int -> unit Js.meth
  method disconnect : unit Js.meth
end

and audioParam = object
  inherit audioNode
  method defaultValue : float Js.readonly_prop
  method value : float Js.prop

  method cancelScheduledValues : float -> unit Js.meth
  method exponentialRampToValueAtTime : float -> float -> unit Js.meth
  method linearRampToValueAtTime : float -> float -> unit Js.meth
  method setTargetAtTime : float -> float -> float -> unit Js.meth
  method setValueAtTime : float -> float -> unit Js.meth
  method setValueCurveAtTime :
    Typed_array.float32Array Js.t -> float -> float -> unit Js.meth
end

and analyserNode = object
  inherit audioNode

  method fftSize : int Js.prop
  method frequencyBinCount : int Js.readonly_prop
  method minDecibels : float Js.prop
  method maxDecibels : float Js.prop
  method smoothingTimeConstant : float Js.prop

  method getFloatFrequencyData : Typed_array.float32Array Js.t -> unit Js.meth
  method getByteFrequencyData : Typed_array.uint8Array Js.t -> unit Js.meth
  method getFloatTimeDomainData : Typed_array.float32Array Js.t -> unit Js.meth
  method getByteTimeDomainData : Typed_array.uint8Array Js.t -> unit Js.meth
end

and audioBufferSourceNode = object
  inherit audioNode

  method buffer : audioBuffer Js.t Js.prop
  method detune : audioParam Js.t Js.readonly_prop
  method loop : bool Js.t Js.prop
  method loopStart : float Js.prop
  method loopEnd : float Js.prop
  method playbackRate : audioParam Js.t Js.readonly_prop

  method onended :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method start : unit Js.meth
  method stop : unit Js.meth
end

and audioDestinationNode = object
  inherit audioNode
  method maxChannelCount : int Js.readonly_prop
end

and biquadFilterNode = object
  inherit audioNode

  method detune : audioParam Js.t Js.prop
  method frequency : audioParam Js.t Js.prop
  method _Q : audioParam Js.t Js.prop
  method _type : Js.js_string Js.t Js.prop

  method gain : audioParam Js.t Js.readonly_prop
end

and channelMergerNode = object
  inherit audioNode
end

and channelSplitterNode = object
  inherit audioNode
end

and convolverNode = object
  inherit audioNode

  method buffer : audioBuffer Js.t Js.prop
  method normalize : bool Js.t Js.prop
end

and delayNode = object
  inherit audioNode

  method delayTime : audioParam Js.t Js.readonly_prop
end

and dynamicsCompressorNode = object
  inherit audioNode

  method reduction : float Js.readonly_prop

  method attack : audioParam Js.t Js.readonly_prop
  method knee : audioParam Js.t Js.readonly_prop
  method ratio : audioParam Js.t Js.readonly_prop
  method release : audioParam Js.t Js.readonly_prop
  method threshold : audioParam Js.t Js.readonly_prop
end

and gainNode = object
  inherit audioNode

  method gain : audioParam Js.t Js.readonly_prop
end

and oscillatorNode = object ('self)
  inherit audioNode

  method detune : audioParam Js.t Js.prop
  method frequency : audioParam Js.t Js.prop
  method _type : Js.js_string Js.t Js.prop

  method start : unit Js.meth
  method stop : unit Js.meth

  method setPeriodicWave : periodicWave Js.t -> unit Js.meth

  method onended :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
end

and stereoPannerNode = object
  inherit audioNode

  method pan : audioParam Js.t Js.readonly_prop
end

and waveShaperNode = object
  inherit audioNode

  method curve : Typed_array.float32Array Js.t Js.prop
  method oversample : Js.js_string Js.t Js.prop
end

and periodicWave = object
end

and audioContext = object
  method currentTime : float Js.readonly_prop
  method destination : audioDestinationNode Js.t Js.readonly_prop
  method sampleRate : float Js.readonly_prop
  method state : Js.js_string Js.t Js.readonly_prop

  method onstatechange :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method close : unit Js.meth
  method resume : unit Js.meth
  method suspend : unit Js.meth

  method createBuffer : int -> int -> float -> audioBuffer Js.t Js.meth

  method createAnalyser : analyserNode Js.t Js.meth
  method createBiquadFilter : biquadFilterNode Js.t Js.meth
  method createBufferSource : audioBufferSourceNode Js.t Js.meth
  method createChannelMerger : int -> channelMergerNode Js.t Js.meth
  method createChannelSplitter : int -> channelSplitterNode Js.t Js.meth
  method createConvolver : convolverNode Js.t Js.meth
  method createDelay : float -> delayNode Js.t Js.meth
  method createDynamicsCompressor : dynamicsCompressorNode Js.t Js.meth
  method createGain : gainNode Js.t Js.meth
  method createOscillator : oscillatorNode Js.t Js.meth
  method createStereoPanner : stereoPannerNode Js.t Js.meth
  method createWaveShaper : waveShaperNode Js.t Js.meth

  method createPeriodicWave :
    Typed_array.float32Array Js.t ->
    Typed_array.float32Array Js.t -> periodicWave Js.t Js.meth

  method decodeAudioData :
    Typed_array.arrayBuffer Js.t -> (audioBuffer Js.t -> unit) -> unit Js.meth
end

val audioContext : audioContext Js.t Js.constr

val is_supported : unit -> bool
