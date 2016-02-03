class type audioContext = object
  method currentTime : int Js.readonly_prop
  method sampleRate : float Js.readonly_prop
end

let audioContext = Js.Unsafe.global##_AudioContext

let is_supported () = Js.Optdef.test audioContext
