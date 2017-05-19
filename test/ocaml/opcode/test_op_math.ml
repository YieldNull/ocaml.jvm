open Core.Std
open OUnit2
open Jvalue
open TestEnv

(* Nulls in long and double args is placeholders *)

let jclass = compile_jclass "test/java/opcode/TestOpMath.java"

let run ?(cmp = (=)) method_name method_descriptor args result =
  let actual = run_method_with_args jclass method_name method_descriptor args in
  assert_equal ~cmp actual (Some result)

let suite =
  "op_math" >:::
  [ "op_iadd" >:: (fun _ -> run "op_iadd" "(II)I" [| Int (-1l); Int 1l |] (Int 0l));
    "op_iadd_underflow" >:: (fun _ -> run "op_iadd" "(II)I" [| Int 0x80000000l; Int 0x80000000l |] (Int 0l));
    "op_iadd_overflow" >:: (fun _ -> run "op_iadd" "(II)I" [| Int 0x7fffffffl; Int 0x7fffffffl |] (Int (-2l)));

    "op_ladd" >:: (fun _ -> run "op_ladd" "(JJ)J" [| Long (-1L); Null; Long 1L |] (Long 0L));
    "op_ladd_underflow" >:: (fun _ -> run "op_ladd" "(JJ)J" [| Long 0x8000000000000000L; Null; Long 0x8000000000000000L |] (Long 0L));
    "op_ladd_overflow" >:: (fun _ -> run "op_ladd" "(JJ)J" [| Long 0x7fffffffffffffffL; Null; Long 0x7fffffffffffffffL |] (Long (-2L)));

    "op_fadd_nan_x" >:: (fun _ -> run ~cmp:compare_float_nan "op_fadd" "(FF)F"
                            [| Float Float32.nan; Float Float32.one |] (Float Float32.nan));
    "op_fadd_nan_nan" >:: (fun _ -> run ~cmp:compare_float_nan "op_fadd" "(FF)F"
                              [| Float Float32.nan; Float Float32.nan |] (Float Float32.nan));

    "op_fadd_posinf_posinf" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                    [| Float Float32.positive_infinity; Float Float32.positive_infinity |] (Float Float32.positive_infinity));
    "op_fadd_neginf_neginf" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                    [| Float Float32.negative_infinity; Float Float32.negative_infinity |] (Float Float32.negative_infinity));

    "op_fadd_posinf_neginf" >:: (fun _ -> run ~cmp:compare_float_nan "op_fadd" "(FF)F"
                                    [| Float Float32.positive_infinity; Float Float32.negative_infinity |] (Float Float32.nan));
    "op_fadd_neginf_posinf" >:: (fun _ -> run ~cmp:compare_float_nan "op_fadd" "(FF)F"
                                    [| Float Float32.negative_infinity; Float Float32.positive_infinity |] (Float Float32.nan));

    "op_fadd_posinf_finite" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                    [| Float Float32.positive_infinity; Float Float32.one |] (Float Float32.positive_infinity));
    "op_fadd_neginf_finite" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                    [| Float Float32.negative_infinity; Float Float32.one |] (Float Float32.negative_infinity));

    "op_fadd_poszero_negzero" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                      [| Float (Float32.of_int32_bits 0x0l); Float (Float32.of_int32_bits 0x80000000l) |]
                                      (Float (Float32.of_int32_bits 0x0l)));
    "op_fadd_poszero_poszero" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                      [| Float (Float32.of_int32_bits 0x0l); Float (Float32.of_int32_bits 0x0l) |]
                                      (Float (Float32.of_int32_bits 0x0l)));
    "op_fadd_negzero_negzero" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                      [| Float (Float32.of_int32_bits 0x80000000l); Float (Float32.of_int32_bits 0x80000000l) |]
                                      (Float (Float32.of_int32_bits 0x80000000l)));

    "op_fadd_negzero_finite" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                     [| Float (Float32.of_int32_bits 0x80000000l); Float Float32.one |]
                                     (Float Float32.one));
    "op_fadd_poszero_finite" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                     [| Float (Float32.of_int32_bits 0x0l); Float Float32.one |]
                                     (Float Float32.one));

    "op_fadd_same_magnitude" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                                     [| Float (Float32.of_int32_bits 0x60012345l); Float (Float32.of_int32_bits 0xe0012345l) |]
                                     (Float Float32.zero));

    (* 3.1415926 + 2.71828182 *)
    "op_fadd" >:: (fun _ -> run ~cmp:compare_f32 "op_fadd" "(FF)F"
                      [| Float (Float32.of_int32_bits 0x40490fdal); Float (Float32.of_int32_bits 0x402df854l) |]
                      (Float (Float32.of_int32_bits 0x40bb8417l)));

    (* 3.1415926 + 2.71828182 *)
    "op_dadd" >:: (fun _ -> run "op_dadd" "(DD)D"
                      [| Double (float_of_string "0x1.5bf0a89f1b0ddp1"); Null; Double (float_of_string "0x1.921fb4d12d84ap1") |]
                      (Double (float_of_string "0x1.77082eb824494p2")));

    "op_isub" >:: (fun _ -> run "op_isub" "(II)I" [| Int (-1l); Int 1l |] (Int (-2l)));
    "op_lsub" >:: (fun _ -> run "op_lsub" "(JJ)J" [| Long (-1L); Null; Long 1L |] (Long (-2L)));
    "op_fsub" >:: (fun _ -> run ~cmp:compare_f32 "op_fsub" "(FF)F"
                      [| Float (Float32.of_int32 (-1l)); Float Float32.one |] (Float (Float32.of_int32 (-2l))));
    "op_dsub" >:: (fun _ -> run "op_dsub" "(DD)D" [| Double (-1.0); Null; Double 1.0 |] (Double (-2.0)));

    "op_imul" >:: (fun _ -> run "op_imul" "(II)I" [| Int (-1l); Int (-1l) |] (Int 1l));
    "op_lmul" >:: (fun _ -> run "op_lmul" "(JJ)J" [| Long (-1L); Null; Long (-1L) |] (Long 1L));
    "op_fmul" >:: (fun _ -> run ~cmp:compare_f32 "op_fmul" "(FF)F"
                      [| Float (Float32.of_int32 (-1l)); Float (Float32.of_int32 (-1l)) |] (Float Float32.one));
    "op_dmul" >:: (fun _ -> run "op_dmul" "(DD)D" [| Double (-1.0); Null; Double (-1.0) |] (Double 1.0));

    "op_idiv" >:: (fun _ -> run "op_idiv" "(II)I" [| Int 3l; Int 2l |] (Int 1l));
    "op_ldiv" >:: (fun _ -> run "op_ldiv" "(JJ)J" [| Long 3L; Null; Long 2L |] (Long 1L));
    "op_fdiv" >:: (fun _ -> run ~cmp:compare_f32 "op_fdiv" "(FF)F"
                      [| Float (Float32.of_int32 3l); Float (Float32.of_int32 2l) |] (Float (Float32.of_int32_bits 0x3fc00000l)));
    "op_ddiv" >:: (fun _ -> run "op_ddiv" "(DD)D" [| Double 3.0; Null; Double 2.0 |] (Double 1.5));
    "op_ldiv_special" >:: (fun _ -> run "op_ldiv" "(JJ)J" [| Long Prim.min_jlong; Null; Long (-1L) |] (Long Prim.min_jlong));

    "op_irem" >:: (fun _ -> run "op_irem" "(II)I" [| Int 3l; Int 2l |] (Int 1l));
    "op_lrem" >:: (fun _ -> run "op_lrem" "(JJ)J" [| Long 3L; Null; Long 2L |] (Long 1L));
    "op_frem" >:: (fun _ -> run ~cmp:compare_f32 "op_frem" "(FF)F" (*3.1 % 2.9*)
                      [| Float (Float32.of_int32_bits 0x40466666l); Float (Float32.of_int32_bits 0x4039999al) |]
                      (Float (Float32.of_int32_bits 0x3e4cccc0l)));
    "op_drem" >:: (fun _ -> run "op_drem" "(DD)D" [| Double 3.1; Null; Double 2.9 |] (Double (float_of_string "0x1.99999999999ap-3")));
    "op_lrem_special" >:: (fun _ -> run "op_lrem" "(JJ)J" [| Long Prim.min_jlong; Null; Long (-1L) |] (Long 0L));

    "op_ineg" >:: (fun _ -> run "op_ineg" "(I)I" [| Int 3l |] (Int (-3l)));
    "op_lneg" >:: (fun _ -> run "op_lneg" "(J)J" [| Long 3L |] (Long (-3L)));
    "op_fneg" >:: (fun _ -> run ~cmp:compare_f32 "op_fneg" "(F)F" (*3.1 % 2.9*)
                      [| Float (Float32.zero) |] (Float (Float32.of_int32_bits 0x80000000l)));
    "op_dneg" >:: (fun _ -> run "op_dneg" "(D)D" [| Double 3.14 |] (Double (-3.14)));


    "op_ishr" >:: (fun _ -> run "op_ishr" "(II)I" [| Int (-1024l); Int 0x7f8903l |] (Int (-128l)));
    "op_lshr" >:: (fun _ -> run "op_lshr" "(JI)J" [| Long (-1024L); Null; Int 0x7f8903l |] (Long (-128L)));

    "op_iushr" >:: (fun _ -> run "op_iushr" "(II)I" [| Int (-1024l); Int 0x7f8903l |] (Int 0x1fffff80l));
    "op_lushr" >:: (fun _ -> run "op_lushr" "(JI)J" [| Long (-1024L); Null; Int 0x7f8903l |] (Long 0x1fffffffffffff80L));

    "op_iand" >:: (fun _ -> run "op_iand" "(II)I" [| Int 0x12345l; Int 0xfffffl |] (Int 0x12345l));
    "op_land" >:: (fun _ -> run "op_land" "(JJ)J" [| Long 0x12345L; Null; Long 0xfffffL |] (Long 0x12345L));

    "op_ior" >:: (fun _ -> run "op_ior" "(II)I" [| Int 0x12345l; Int 0xfffffl |] (Int 0xfffffl));
    "op_lor" >:: (fun _ -> run "op_lor" "(JJ)J" [| Long 0x12345L; Null; Long 0xfffffL |] (Long 0xfffffL));

    "op_ixor" >:: (fun _ -> run "op_ixor" "(II)I" [| Int 0x12345l; Int 0x12345l |] (Int 0l));
    "op_lxor" >:: (fun _ -> run "op_lxor" "(JJ)J" [| Long 0x12345L; Null; Long 0x12345L |] (Long 0L));

    "op_iinc" >:: (fun _ -> run "op_iinc" "(I)I" [| Int 0x7fffffffl |] (Int Prim.min_jint));
  ]

let () =
  run_test_tt_main suite
