include Op_comparisons
include Op_constants
include Op_control
include Op_conversions
include Op_extended
include Op_loads
include Op_math
include Op_references
include Op_stack
include Op_stores

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
  (* | 0xac -> op_ireturn
     | 0xad -> op_lreturn
     | 0xae -> op_freturn
     | 0xaf -> op_dreturn
     | 0xb0 -> op_areturn
     | 0xb1 -> op_return *)
  (* | 0xb2 -> op_getstatic
     | 0xb3 -> op_putstatic *)
  | 0xb4 -> op_getfield
  | 0xb5 -> op_putfield
  (* | 0xb6 -> op_invokevirtual
     | 0xb7 -> op_invokespecial
     | 0xb8 -> op_invokestatic
     | 0xb9 -> op_invokeinterface
     | 0xba -> op_invokedynamic *)
  (* | 0xbb -> op_new *)
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
  (* | 0xca -> op_breakpoint
     | 0xfe -> op_impdep1
     | 0xff -> op_impdep2 *)
  | _ -> failwith ""
