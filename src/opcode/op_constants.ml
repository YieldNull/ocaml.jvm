open Core
open VMError
open Jvalue
open Frame

let op_nop _ = ()
let op_aconst_null frame = stack_push frame Null

let op_iconst_m1 frame = stack_push frame (Int (-1l))
let op_iconst_0 frame = stack_push frame (Int 0l)
let op_iconst_1 frame = stack_push frame (Int 1l)
let op_iconst_2 frame = stack_push frame (Int 2l)
let op_iconst_3 frame = stack_push frame (Int 3l)
let op_iconst_4 frame = stack_push frame (Int 4l)
let op_iconst_5 frame = stack_push frame (Int 5l)

let op_lconst_0 frame = stack_push frame (Long 0L)
let op_lconst_1 frame = stack_push frame (Long 1L)
let op_fconst_0 frame = stack_push frame (Float Float32.zero)
let op_fconst_1 frame = stack_push frame (Float Float32.one)
let op_fconst_2 frame = stack_push frame (Float (Float32.add Float32.one Float32.one))
let op_dconst_0 frame = stack_push frame (Double 0.0)
let op_dconst_1 frame = stack_push frame (Double 1.0)

let op_bipush frame = stack_push frame (Int (Caml.Int32.of_int @@ read_byte frame))
let op_sipush frame = stack_push frame (Int (Caml.Int32.of_int @@ read_i16 frame))

let ldc_common frame index =
  match Poolrt.get (conspool frame) index with
  | Poolrt.Integer x -> stack_push frame (Int x)
  | Poolrt.Float f -> stack_push frame (Float f)
  | Poolrt.String s ->
    let strobj = Jstring.find_or_create s in
    stack_push frame (Object strobj)
  | Poolrt.Class c -> () (* TODO *)
  | Poolrt.MethodType mt -> () (* TODO *)
  | Poolrt.MethodHandle mh -> () (* TODO *)
  | _ -> raise (ClassFormatError "Invalid index")

let op_ldc frame = ldc_common frame (read_byte frame)

let op_ldc_w frame = ldc_common frame (read_ui16 frame)

let op_ldc2_w frame =
  let index = read_ui16 frame in
  match Poolrt.get (conspool frame) index with
  | Poolrt.Long l -> stack_push frame (Long l)
  | Poolrt.Double d -> stack_push frame (Double d)
  | _ -> raise (ClassFormatError "Invalid index")
