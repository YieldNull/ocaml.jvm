open Core.Std

(* https://github.com/dmlloyd/openjdk/blob/jdk8u/jdk8u/hotspot/src/share/vm/runtime/sharedRuntime.cpp#L333 *)

let max_jint = 0x7fffffffl

let min_jint = 0x80000000l

let max_jlong = 0x7fffffffffffffffL

let min_jlong = 0x8000000000000000L

let c2i x = x

let b2i x =
  let open Int32 in
  if bit_and 0x80l x = 0l then x (* positive *)
  else x + 0xffffff00l

let s2i x =
  let open Int32 in
  if bit_and 0x8000l x = 0l then x (* positive *)
  else x + 0xffff0000l

let i2b x = b2i @@ Int32.bit_and 0xffl x

let i2c x = Int32.bit_and 0xffffl x

let i2s x = s2i @@ Int32.bit_and 0xffffl x

let i2f x = Float32.of_int32 x

let i2l x = Int32.to_int64 x

let i2d x = Int32.to_float x

let l2i x = Caml.Int64.to_int32 x

let l2f x = Float32.of_int64 x

let l2d x = Int64.to_float x

let f2i x =
  let open Float32 in
  if is_nan x then 0l
  else if x >=. i2f max_jint then max_jint
  else if x <=. i2f min_jint then min_jint
  else to_int32 x

let f2l x =
  let open Float32 in
  if is_nan x then 0L
  else if x >=. l2f max_jlong then max_jlong
  else if x <=. l2f min_jlong then min_jlong
  else to_int64 x

let f2d x = Float32.to_float64 x

let d2i x =
  if Float.is_nan x then 0l
  else if x >=. i2d max_jint then max_jint
  else if x <=. i2d min_jint then min_jint
  else Int32.of_float x

let d2f x = Float32.of_float64 x

let d2l x =
  if Float.is_nan x then 0L
  else if x >=. l2d max_jlong then max_jlong
  else if x <=. l2d min_jlong then min_jlong
  else Int64.of_float x
