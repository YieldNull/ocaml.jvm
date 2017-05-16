open Core.Std
open VMError
open Jvalue
open Frame

let op_nop _ = ()
let op_aconst_null frame = Stack.push frame.opstack (Reference Jobject.Null)

let op_iconst_m1 frame = Stack.push frame.opstack (Int (-1l))
let op_iconst_0 frame = Stack.push frame.opstack (Int 0l)
let op_iconst_1 frame = Stack.push frame.opstack (Int 1l)
let op_iconst_2 frame = Stack.push frame.opstack (Int 2l)
let op_iconst_3 frame = Stack.push frame.opstack (Int 3l)
let op_iconst_4 frame = Stack.push frame.opstack (Int 4l)
let op_iconst_5 frame = Stack.push frame.opstack (Int 5l)

let op_lconst_0 frame = Stack.push frame.opstack (Long 0L)
let op_lconst_1 frame = Stack.push frame.opstack (Long 1L)
let op_fconst_0 frame = Stack.push frame.opstack (Float Float32.zero)
let op_fconst_1 frame = Stack.push frame.opstack (Float Float32.one)
let op_fconst_2 frame = Stack.push frame.opstack (Float (Float32.add Float32.one Float32.one))
let op_dconst_0 frame = Stack.push frame.opstack (Double 0.0)
let op_dconst_1 frame = Stack.push frame.opstack (Double 1.0)

let op_bipush frame = Stack.push frame.opstack (Int (Caml.Int32.of_int @@ read_byte frame))
let op_sipush frame = Stack.push frame.opstack (Int (Caml.Int32.of_int @@ read_i16 frame))

let ldc_common frame index =
  match load_conspool frame index with
  | Poolrt.Integer x -> Stack.push frame.opstack (Int x)
  | Poolrt.Float f -> Stack.push frame.opstack (Float f)
  | Poolrt.Class c -> Stack.push frame.opstack (Reference (Jobject.Obj c))
  | Poolrt.String s -> Stack.push frame.opstack (Reference (Jobject.Obj s))
  | Poolrt.UnresolvedString s ->
    let strobj = Jstring.find_or_create s in
    set_conspool frame index (Poolrt.String strobj);
    Stack.push frame.opstack (Reference (Jobject.Obj (strobj)))
  | Poolrt.UnresolvedClass c -> () (* TODO *)
  | Poolrt.MethodType mt -> () (* TODO *)
  | Poolrt.MethodHandle mh -> () (* TODO *)
  | _ -> raise (ClassFormatError "Invalid index")

let op_ldc frame = ldc_common frame (read_byte frame)

let op_ldc_w frame = ldc_common frame (read_ui16 frame)

let op_ldc2_w frame =
  let index = read_ui16 frame in
  match load_conspool frame index with
  | Poolrt.Long l -> Stack.push frame.opstack (Long l)
  | Poolrt.Double d -> Stack.push frame.opstack (Double d)
  | _ -> raise (ClassFormatError "Invalid index")
