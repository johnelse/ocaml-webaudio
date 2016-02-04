class type audioParam = object
  method defaultValue : float Js.readonly_prop
  method value : float Js.prop
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
  method currentTime : int Js.readonly_prop
  method destination : audioDestinationNode Js.t Js.readonly_prop
  method sampleRate : float Js.readonly_prop

  method createOscillator : oscillatorNode Js.t Js.meth
end

let audioContext = Js.Unsafe.global##_AudioContext

let is_supported () = Js.Optdef.test audioContext
