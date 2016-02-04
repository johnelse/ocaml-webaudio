class type audioParam = object
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

class type audioNode = object
  method context : audioContext Js.t Js.readonly_prop
  method numberOfInputs : int Js.readonly_prop
  method numberOfOutputs : int Js.readonly_prop

  method connect : audioNode Js.t -> unit Js.meth
  method disconnect : unit Js.meth
end

and audioDestinationNode = object
  inherit audioNode
end

and oscillatorNode = object
  inherit audioNode

  method frequency : audioParam Js.t Js.prop
  method _type : Js.js_string Js.t Js.prop

  method start : unit Js.meth
  method stop : unit Js.meth
end

and audioContext = object
  method currentTime : float Js.readonly_prop
  method destination : audioDestinationNode Js.t Js.readonly_prop
  method sampleRate : float Js.readonly_prop

  method createOscillator : oscillatorNode Js.t Js.meth
end

val audioContext : audioContext Js.t Js.constr

val is_supported : unit -> bool
