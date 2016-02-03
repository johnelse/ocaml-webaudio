class type audioContext = object
  method currentTime : int Js.readonly_prop
  method sampleRate : float Js.readonly_prop
end

val audioContext : audioContext Js.t Js.constr

val is_supported : unit -> bool
