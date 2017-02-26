open Core.Std
open Attribute
open BatIO
open BatIO.BigEndian
open VMError
open Float32
open Jvalue

type t =
  { jmethod : Jmethod.t;
    localvars : Jvalue.t array;
    opstack : (Jvalue.t) Stack.t;
  }

let create jmethod localvar_initializer =
  let rec aux = function
    | [] -> failwith ""
    | hd :: tail -> match hd with
      | AttrMethod.Code code -> code
      | _ -> aux tail
  in
  let code = aux jmethod.Jmethod.attrs in
  let localvars = Array.init code.Code.max_locals ~f:localvar_initializer in
  let opstack = Stack.create () in
  { jmethod; localvars; opstack; }

let get_conspool t = Jclass.conspool @@ Jmethod.get_class t.jmethod

let op_nop t input = ()
let op_aconst_null t input = Stack.push t.opstack (Reference Jobject.Null)

let op_iconst_m1 t input = Stack.push t.opstack (Int (Int32.of_int_exn (-1)))
let op_iconst_0 t input = Stack.push t.opstack (Int (Int32.of_int_exn 0))
let op_iconst_1 t input = Stack.push t.opstack (Int (Int32.of_int_exn 1))
let op_iconst_2 t input = Stack.push t.opstack (Int (Int32.of_int_exn 2))
let op_iconst_3 t input = Stack.push t.opstack (Int (Int32.of_int_exn 3))
let op_iconst_4 t input = Stack.push t.opstack (Int (Int32.of_int_exn 4))
let op_iconst_5 t input = Stack.push t.opstack (Int (Int32.of_int_exn 5))

let op_lconst_0 t input = Stack.push t.opstack (Long (Int64.of_int 0))
let op_lconst_1 t input = Stack.push t.opstack (Long (Int64.of_int 1))
let op_fconst_0 t input = Stack.push t.opstack (Float 0.0)
let op_fconst_1 t input = Stack.push t.opstack (Float 1.0)
let op_fconst_2 t input = Stack.push t.opstack (Float 2.0)
let op_dconst_0 t input = Stack.push t.opstack (Double 0.0)
let op_dconst_1 t input = Stack.push t.opstack (Double 1.0)

let op_bipush t input = Stack.push t.opstack (Int (Int32.of_int_exn @@ read_byte input))
let op_sipush t input = Stack.push t.opstack (Int (Int32.of_int_exn @@ read_i16 input))

let op_ldc t input = ()
let op_ldc_w t input = ()
let op_ldc2_w t input = ()

let op_iload t input = Stack.push t.opstack (Int (get_int t.localvars.(read_byte input)))
let op_lload t input = Stack.push t.opstack (Long (get_long t.localvars.(read_byte input)))
let op_fload t input = Stack.push t.opstack (Float (get_float t.localvars.(read_byte input)))
let op_dload t input = Stack.push t.opstack (Double (get_double t.localvars.(read_byte input)))
let op_aload t input = Stack.push t.opstack (Reference (get_reference t.localvars.(read_byte input)))

let op_iload_0 t input = Stack.push t.opstack (Int (get_int t.localvars.(0)))
let op_iload_1 t input = Stack.push t.opstack (Int (get_int t.localvars.(1)))
let op_iload_2 t input = Stack.push t.opstack (Int (get_int t.localvars.(2)))
let op_iload_3 t input = Stack.push t.opstack (Int (get_int t.localvars.(3)))

let op_lload_0 t input = Stack.push t.opstack (Long (get_long t.localvars.(0)))
let op_lload_1 t input = Stack.push t.opstack (Long (get_long t.localvars.(1)))
let op_lload_2 t input = Stack.push t.opstack (Long (get_long t.localvars.(2)))
let op_lload_3 t input = Stack.push t.opstack (Long (get_long t.localvars.(3)))

let op_fload_0 t input = Stack.push t.opstack (Float (get_float t.localvars.(0)))
let op_fload_1 t input = Stack.push t.opstack (Float (get_float t.localvars.(1)))
let op_fload_2 t input = Stack.push t.opstack (Float (get_float t.localvars.(2)))
let op_fload_3 t input = Stack.push t.opstack (Float (get_float t.localvars.(3)))

let op_dload_0 t input = Stack.push t.opstack (Double (get_double t.localvars.(0)))
let op_dload_1 t input = Stack.push t.opstack (Double (get_double t.localvars.(1)))
let op_dload_2 t input = Stack.push t.opstack (Double (get_double t.localvars.(2)))
let op_dload_3 t input = Stack.push t.opstack (Double (get_double t.localvars.(3)))

let op_aload_0 t input = Stack.push t.opstack (Reference (get_reference t.localvars.(0)))
let op_aload_1 t input = Stack.push t.opstack (Reference (get_reference t.localvars.(1)))
let op_aload_2 t input = Stack.push t.opstack (Reference (get_reference t.localvars.(2)))
let op_aload_3 t input = Stack.push t.opstack (Reference (get_reference t.localvars.(3)))

let op_iaload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_int @@ Jobject.load arr index in
  Stack.push t.opstack (Int value)

let op_laload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_long @@ Jobject.load arr index in
  Stack.push t.opstack (Long value)

let op_faload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_float @@ Jobject.load arr index in
  Stack.push t.opstack (Float value)

let op_daload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_double @@ Jobject.load arr index in
  Stack.push t.opstack (Double value)

let op_aaload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_reference @@ Jobject.load arr index in
  Stack.push t.opstack (Reference value)

let op_baload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_byte @@ Jobject.load arr index in
  Stack.push t.opstack (Byte value)

let op_caload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_char @@ Jobject.load arr index in
  Stack.push t.opstack (Char value)

let op_saload t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = get_short @@ Jobject.load arr index in
  Stack.push t.opstack (Short value)

let op_istore t input = t.localvars.(read_byte input) <- Int (get_int @@ Stack.pop_exn t.opstack)
let op_lstore t input = t.localvars.(read_byte input) <- Long (get_long @@ Stack.pop_exn t.opstack)
let op_fstore t input = t.localvars.(read_byte input) <- Float (get_float @@ Stack.pop_exn t.opstack)
let op_dstore t input = t.localvars.(read_byte input) <- Double (get_double @@ Stack.pop_exn t.opstack)
let op_astore t input = t.localvars.(read_byte input) <- Reference (get_reference @@ Stack.pop_exn t.opstack)

let op_istore_0 t input = t.localvars.(0) <- Int (get_int @@ Stack.pop_exn t.opstack)
let op_istore_1 t input = t.localvars.(1) <- Int (get_int @@ Stack.pop_exn t.opstack)
let op_istore_2 t input = t.localvars.(2) <- Int (get_int @@ Stack.pop_exn t.opstack)
let op_istore_3 t input = t.localvars.(3) <- Int (get_int @@ Stack.pop_exn t.opstack)

let op_lstore_0 t input = t.localvars.(0) <- Long (get_long @@ Stack.pop_exn t.opstack)
let op_lstore_1 t input = t.localvars.(1) <- Long (get_long @@ Stack.pop_exn t.opstack)
let op_lstore_2 t input = t.localvars.(2) <- Long (get_long @@ Stack.pop_exn t.opstack)
let op_lstore_3 t input = t.localvars.(3) <- Long (get_long @@ Stack.pop_exn t.opstack)

let op_fstore_0 t input = t.localvars.(0) <- Float (get_float @@ Stack.pop_exn t.opstack)
let op_fstore_1 t input = t.localvars.(1) <- Float (get_float @@ Stack.pop_exn t.opstack)
let op_fstore_2 t input = t.localvars.(2) <- Float (get_float @@ Stack.pop_exn t.opstack)
let op_fstore_3 t input = t.localvars.(3) <- Float (get_float @@ Stack.pop_exn t.opstack)

let op_dstore_0 t input = t.localvars.(0) <- Double (get_double @@ Stack.pop_exn t.opstack)
let op_dstore_1 t input = t.localvars.(1) <- Double (get_double @@ Stack.pop_exn t.opstack)
let op_dstore_2 t input = t.localvars.(2) <- Double (get_double @@ Stack.pop_exn t.opstack)
let op_dstore_3 t input = t.localvars.(3) <- Double (get_double @@ Stack.pop_exn t.opstack)

let op_astore_0 t input = t.localvars.(0) <- Reference (get_reference @@ Stack.pop_exn t.opstack)
let op_astore_1 t input = t.localvars.(1) <- Reference (get_reference @@ Stack.pop_exn t.opstack)
let op_astore_2 t input = t.localvars.(2) <- Reference (get_reference @@ Stack.pop_exn t.opstack)
let op_astore_3 t input = t.localvars.(3) <- Reference (get_reference @@ Stack.pop_exn t.opstack)

let op_iastore t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let index = get_int @@ Stack.pop_exn t.opstack in
  let value = Stack.pop_exn t.opstack in
  Jobject.store arr index value

let op_lastore t input = op_iastore t input
let op_fastore t input = op_iastore t input
let op_dastore t input = op_iastore t input
let op_aastore t input = op_iastore t input
let op_bastore t input = op_iastore t input
let op_castore t input = op_iastore t input
let op_sastore t input = op_iastore t input

let op_pop t input = ()
let op_pop2 t input = ()
let op_dup t input = ()
let op_dup_x1 t input = ()
let op_dup_x2 t input = ()
let op_dup2 t input = ()
let op_dup2_x1 t input = ()
let op_dup2_x2 t input = ()
let op_swap t input = ()
let op_iadd t input = ()
let op_ladd t input = ()
let op_fadd t input = ()
let op_dadd t input = ()
let op_isub t input = ()
let op_lsub t input = ()
let op_fsub t input = ()
let op_dsub t input = ()
let op_imul t input = ()
let op_lmul t input = ()
let op_fmul t input = ()
let op_dmul t input = ()
let op_idiv t input = ()
let op_ldiv t input = ()
let op_fdiv t input = ()
let op_ddiv t input = ()
let op_irem t input = ()
let op_lrem t input = ()
let op_frem t input = ()
let op_drem t input = ()
let op_ineg t input = ()
let op_lneg t input = ()
let op_fneg t input = ()
let op_dneg t input = ()
let op_ishl t input = ()
let op_lshl t input = ()
let op_ishr t input = ()
let op_lshr t input = ()
let op_iushr t input = ()
let op_lushr t input = ()
let op_iand t input = ()
let op_land t input = ()
let op_ior t input = ()
let op_lor t input = ()
let op_ixor t input = ()
let op_lxor t input = ()
let op_iinc t input = ()
let op_i2l t input = ()
let op_i2f t input = ()
let op_i2d t input = ()
let op_l2i t input = ()
let op_l2f t input = ()
let op_l2d t input = ()
let op_f2i t input = ()
let op_f2l t input = ()
let op_f2d t input = ()
let op_d2i t input = ()
let op_d2l t input = ()
let op_d2f t input = ()
let op_i2b t input = ()
let op_i2c t input = ()
let op_i2s t input = ()
let op_lcmp t input = ()
let op_fcmpl t input = ()
let op_fcmpg t input = ()
let op_dcmpl t input = ()
let op_dcmpg t input = ()
let op_ifeq t input = ()
let op_ifne t input = ()
let op_iflt t input = ()
let op_ifge t input = ()
let op_ifgt t input = ()
let op_ifle t input = ()
let op_if_icmpeq t input = ()
let op_if_icmpne t input = ()
let op_if_icmplt t input = ()
let op_if_icmpge t input = ()
let op_if_icmpgt t input = ()
let op_if_icmple t input = ()
let op_if_acmpeq t input = ()
let op_if_acmpne t input = ()
let op_goto t input = ()
let op_jsr t input = ()
let op_ret t input = ()
let op_tableswitch t input = ()
let op_lookupswitch t input = ()
let op_ireturn t input = ()
let op_lreturn t input = ()
let op_freturn t input = ()
let op_dreturn t input = ()
let op_areturn t input = ()
let op_return t input = ()
let op_getstatic t input = ()
let op_putstatic t input = ()
let op_getfield t input = ()
let op_putfield t input = ()
let op_invokevirtual t input = ()
let op_invokespecial t input = ()
let op_invokestatic t input = ()
let op_invokeinterface t input = ()
let op_invokedynamic t input = ()

let op_new t input =
  let conspool = get_conspool t in
  let index = read_ui16 input in
  let jclass = Poolrt.get_class conspool index in
  let obj = Jobject.create_obj jclass in
  Stack.push t.opstack (Reference obj)

let op_newarray t input =
  let count = get_int @@ Stack.pop_exn t.opstack in
  let arr = Jobject.create_arr count (read_byte input) in
  Stack.push t.opstack (Reference arr)

let op_anewarray t input =
  let index = read_ui16 input in
  let count = get_int @@ Stack.pop_exn t.opstack in
  let conspool = get_conspool t in
  let jclass = Poolrt.get_class conspool index in
  let arr = Jobject.create_multi_arr jclass count in
  Stack.push t.opstack (Reference arr)

let op_arraylength t input =
  let arr = get_reference @@ Stack.pop_exn t.opstack in
  let len = Jobject.get_array_length arr in
  Stack.push t.opstack (Int len)

let op_athrow t input = ()
let op_checkcast t input = ()
let op_instanceof t input = ()
let op_monitorenter t input = ()
let op_monitorexit t input = ()
let op_wide t input = ()
let op_multianewarray t input = ()
let op_ifnull t input = ()
let op_ifnonnull t input = ()
let op_goto_w t input = ()
let op_jsr_w t input = ()
let op_breakpoint t input = ()
let op_impdep1 t input = ()
let op_impdep2 t input = ()


let opcode_to_func = function
  | 0x00 -> op_nop
  | 0x01 -> op_aconst_null
  | 0x02 -> op_iconst_m1
  | 0x03 -> op_iconst_0
  | 0x04 -> op_iconst_1
  | 0x05 -> op_iconst_2
  | 0x06 -> op_iconst_3
  | 0x07 -> op_iconst_4
  | 0x08 -> op_iconst_5
  | 0x09 -> op_lconst_0
  | 0x0a -> op_lconst_1
  | 0x0b -> op_fconst_0
  | 0x0c -> op_fconst_1
  | 0x0d -> op_fconst_2
  | 0x0e -> op_dconst_0
  | 0x0f -> op_dconst_1
  | 0x10 -> op_bipush
  | 0x11 -> op_sipush
  | 0x12 -> op_ldc
  | 0x13 -> op_ldc_w
  | 0x14 -> op_ldc2_w
  | 0x15 -> op_iload
  | 0x16 -> op_lload
  | 0x17 -> op_fload
  | 0x18 -> op_dload
  | 0x19 -> op_aload
  | 0x1a -> op_iload_0
  | 0x1b -> op_iload_1
  | 0x1c -> op_iload_2
  | 0x1d -> op_iload_3
  | 0x1e -> op_lload_0
  | 0x1f -> op_lload_1
  | 0x20 -> op_lload_2
  | 0x21 -> op_lload_3
  | 0x22 -> op_fload_0
  | 0x23 -> op_fload_1
  | 0x24 -> op_fload_2
  | 0x25 -> op_fload_3
  | 0x26 -> op_dload_0
  | 0x27 -> op_dload_1
  | 0x28 -> op_dload_2
  | 0x29 -> op_dload_3
  | 0x2a -> op_aload_0
  | 0x2b -> op_aload_1
  | 0x2c -> op_aload_2
  | 0x2d -> op_aload_3
  | 0x2e -> op_iaload
  | 0x2f -> op_laload
  | 0x30 -> op_faload
  | 0x31 -> op_daload
  | 0x32 -> op_aaload
  | 0x33 -> op_baload
  | 0x34 -> op_caload
  | 0x35 -> op_saload
  | 0x36 -> op_istore
  | 0x37 -> op_lstore
  | 0x38 -> op_fstore
  | 0x39 -> op_dstore
  | 0x3a -> op_astore
  | 0x3b -> op_istore_0
  | 0x3c -> op_istore_1
  | 0x3d -> op_istore_2
  | 0x3e -> op_istore_3
  | 0x3f -> op_lstore_0
  | 0x40 -> op_lstore_1
  | 0x41 -> op_lstore_2
  | 0x42 -> op_lstore_3
  | 0x43 -> op_fstore_0
  | 0x44 -> op_fstore_1
  | 0x45 -> op_fstore_2
  | 0x46 -> op_fstore_3
  | 0x47 -> op_dstore_0
  | 0x48 -> op_dstore_1
  | 0x49 -> op_dstore_2
  | 0x4a -> op_dstore_3
  | 0x4b -> op_astore_0
  | 0x4c -> op_astore_1
  | 0x4d -> op_astore_2
  | 0x4e -> op_astore_3
  | 0x4f -> op_iastore
  | 0x50 -> op_lastore
  | 0x51 -> op_fastore
  | 0x52 -> op_dastore
  | 0x53 -> op_aastore
  | 0x54 -> op_bastore
  | 0x55 -> op_castore
  | 0x56 -> op_sastore
  | 0x57 -> op_pop
  | 0x58 -> op_pop2
  | 0x59 -> op_dup
  | 0x5a -> op_dup_x1
  | 0x5b -> op_dup_x2
  | 0x5c -> op_dup2
  | 0x5d -> op_dup2_x1
  | 0x5e -> op_dup2_x2
  | 0x5f -> op_swap
  | 0x60 -> op_iadd
  | 0x61 -> op_ladd
  | 0x62 -> op_fadd
  | 0x63 -> op_dadd
  | 0x64 -> op_isub
  | 0x65 -> op_lsub
  | 0x66 -> op_fsub
  | 0x67 -> op_dsub
  | 0x68 -> op_imul
  | 0x69 -> op_lmul
  | 0x6a -> op_fmul
  | 0x6b -> op_dmul
  | 0x6c -> op_idiv
  | 0x6d -> op_ldiv
  | 0x6e -> op_fdiv
  | 0x6f -> op_ddiv
  | 0x70 -> op_irem
  | 0x71 -> op_lrem
  | 0x72 -> op_frem
  | 0x73 -> op_drem
  | 0x74 -> op_ineg
  | 0x75 -> op_lneg
  | 0x76 -> op_fneg
  | 0x77 -> op_dneg
  | 0x78 -> op_ishl
  | 0x79 -> op_lshl
  | 0x7a -> op_ishr
  | 0x7b -> op_lshr
  | 0x7c -> op_iushr
  | 0x7d -> op_lushr
  | 0x7e -> op_iand
  | 0x7f -> op_land
  | 0x80 -> op_ior
  | 0x81 -> op_lor
  | 0x82 -> op_ixor
  | 0x83 -> op_lxor
  | 0x84 -> op_iinc
  | 0x85 -> op_i2l
  | 0x86 -> op_i2f
  | 0x87 -> op_i2d
  | 0x88 -> op_l2i
  | 0x89 -> op_l2f
  | 0x8a -> op_l2d
  | 0x8b -> op_f2i
  | 0x8c -> op_f2l
  | 0x8d -> op_f2d
  | 0x8e -> op_d2i
  | 0x8f -> op_d2l
  | 0x90 -> op_d2f
  | 0x91 -> op_i2b
  | 0x92 -> op_i2c
  | 0x93 -> op_i2s
  | 0x94 -> op_lcmp
  | 0x95 -> op_fcmpl
  | 0x96 -> op_fcmpg
  | 0x97 -> op_dcmpl
  | 0x98 -> op_dcmpg
  | 0x99 -> op_ifeq
  | 0x9a -> op_ifne
  | 0x9b -> op_iflt
  | 0x9c -> op_ifge
  | 0x9d -> op_ifgt
  | 0x9e -> op_ifle
  | 0x9f -> op_if_icmpeq
  | 0xa0 -> op_if_icmpne
  | 0xa1 -> op_if_icmplt
  | 0xa2 -> op_if_icmpge
  | 0xa3 -> op_if_icmpgt
  | 0xa4 -> op_if_icmple
  | 0xa5 -> op_if_acmpeq
  | 0xa6 -> op_if_acmpne
  | 0xa7 -> op_goto
  | 0xa8 -> op_jsr
  | 0xa9 -> op_ret
  | 0xaa -> op_tableswitch
  | 0xab -> op_lookupswitch
  | 0xac -> op_ireturn
  | 0xad -> op_lreturn
  | 0xae -> op_freturn
  | 0xaf -> op_dreturn
  | 0xb0 -> op_areturn
  | 0xb1 -> op_return
  | 0xb2 -> op_getstatic
  | 0xb3 -> op_putstatic
  | 0xb4 -> op_getfield
  | 0xb5 -> op_putfield
  | 0xb6 -> op_invokevirtual
  | 0xb7 -> op_invokespecial
  | 0xb8 -> op_invokestatic
  | 0xb9 -> op_invokeinterface
  | 0xba -> op_invokedynamic
  | 0xbb -> op_new
  | 0xbc -> op_newarray
  | 0xbd -> op_anewarray
  | 0xbe -> op_arraylength
  | 0xbf -> op_athrow
  | 0xc0 -> op_checkcast
  | 0xc1 -> op_instanceof
  | 0xc2 -> op_monitorenter
  | 0xc3 -> op_monitorexit
  | 0xc4 -> op_wide
  | 0xc5 -> op_multianewarray
  | 0xc6 -> op_ifnull
  | 0xc7 -> op_ifnonnull
  | 0xc8 -> op_goto_w
  | 0xc9 -> op_jsr_w
  | 0xca -> op_breakpoint
  | 0xfe -> op_impdep1
  | 0xff -> op_impdep2
  | _ -> failwith ""
