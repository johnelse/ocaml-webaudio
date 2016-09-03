class type audioNode = object
  method context : audioContext Js.t Js.readonly_prop
  method numberOfInputs : int Js.readonly_prop
  method numberOfOutputs : int Js.readonly_prop

  method channelCount : int Js.prop
  method channelCountMode : Js.js_string Js.t Js.prop
  method channelInterpretation : Js.js_string Js.t Js.prop

  method connect : audioNode Js.t -> unit Js.meth
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

and oscillatorNode = object ('self)
  inherit audioNode

  method detune : audioParam Js.t Js.prop
  method frequency : audioParam Js.t Js.prop
  method _type : Js.js_string Js.t Js.prop

  method start : unit Js.meth
  method stop : unit Js.meth

  method onended :
    ('self Js.t, Dom_html.event Js.t) Dom_html.event_listener Js.writeonly_prop
end

and audioContext = object
  method currentTime : float Js.readonly_prop
  method destination : audioDestinationNode Js.t Js.readonly_prop
  method sampleRate : float Js.readonly_prop
  method state : Js.js_string Js.t Js.readonly_prop

  method close : unit Js.meth
  method resume : unit Js.meth
  method suspend : unit Js.meth

  method createBiquadFilter : biquadFilterNode Js.t Js.meth
  method createOscillator : oscillatorNode Js.t Js.meth
end

val audioContext : audioContext Js.t Js.constr

val is_supported : unit -> bool
