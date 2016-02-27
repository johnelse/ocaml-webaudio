class type audioNode = object
  method context : audioContext Js.t Js.readonly_prop
  method numberOfInputs : int Js.readonly_prop
  method numberOfOutputs : int Js.readonly_prop

  method channelCount : int Js.prop
  method channelCountMode : Js.js_string Js.t Js.prop
  method channelInterpretation : Js.js_string Js.t Js.prop

  method connect_AudioNode : audioNode Js.t -> unit Js.meth
  method connect_AudioParam : audioParam Js.t -> unit Js.meth
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

and audioDestinationNode = object
  inherit audioNode
end

and oscillatorNode = object
  inherit audioNode

  method detune : audioParam Js.t Js.prop
  method frequency : audioParam Js.t Js.prop
  method _type : Js.js_string Js.t Js.prop

  method start : unit Js.meth
  method stop : unit Js.meth
end

and audioContext = object
  method currentTime : float Js.readonly_prop
  method destination : audioDestinationNode Js.t Js.readonly_prop
  method sampleRate : float Js.readonly_prop
  method state : Js.js_string Js.t Js.readonly_prop

  method createOscillator : oscillatorNode Js.t Js.meth
end

let audioContext =
  Js.Optdef.get
    (Js.Unsafe.global##_AudioContext)
    (fun () -> Js.Unsafe.global##webkitAudioContext)

let is_supported () = Js.Optdef.test audioContext
