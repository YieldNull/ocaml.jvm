open Core.Std
open Jvalue
open Frame

let op_nop _ = ()
let op_aconst_null frame = Stack.push frame.opstack (Reference Jobject.Null)

let op_iconst_m1 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int (-1)))
let op_iconst_0 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int 0))
let op_iconst_1 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int 1))
let op_iconst_2 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int 2))
let op_iconst_3 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int 3))
let op_iconst_4 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int 4))
let op_iconst_5 frame = Stack.push frame.opstack (Int (Caml.Int32.of_int 5))

let op_lconst_0 frame = Stack.push frame.opstack (Long (Int64.of_int 0))
let op_lconst_1 frame = Stack.push frame.opstack (Long (Int64.of_int 1))
let op_fconst_0 frame = Stack.push frame.opstack (Float Float32.zero)
let op_fconst_1 frame = Stack.push frame.opstack (Float Float32.one)
let op_fconst_2 frame = Stack.push frame.opstack (Float (Float32.add Float32.one Float32.one))
let op_dconst_0 frame = Stack.push frame.opstack (Double 0.0)
let op_dconst_1 frame = Stack.push frame.opstack (Double 1.0)

let op_bipush frame = Stack.push frame.opstack (Int (Caml.Int32.of_int @@ read_byte frame))
let op_sipush frame = Stack.push frame.opstack (Int (Caml.Int32.of_int @@ read_i16 frame))

let op_ldc frame =
  let index = read_byte frame in
  match load_conspool frame index with
  | Poolrt.Integer x -> Stack.push frame.opstack (Int x)
  | Poolrt.Float f -> Stack.push frame.opstack (Float f)
  | Poolrt.String s -> Stack.push frame.opstack (Reference (Jobject.Obj (Jstring.find_or_create s)))
  | Poolrt.Class c -> () (* TODO *)
  | Poolrt.MethodType mt -> () (* TODO *)
  | Poolrt.MethodHandle mh -> () (* TODO *)
  | _ -> failwith ""


let op_ldc_w frame = op_ldc frame
let op_ldc2_w frame = ()
