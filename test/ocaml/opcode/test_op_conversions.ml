open Core
open OUnit2
open Jvalue
open TestEnv

let jclass = compile_jclass "test/java/opcode/TestOpConversions.java"

let run ?(cmp = (=)) method_name method_descriptor result =
  let actual = run_method jclass method_name method_descriptor in
  assert_equal ~cmp actual (Some result)

let suite =
  "op_conversions" >:::
  [ "op_i2l" >:: (fun _ -> run "op_i2l" "()J" (Long 0x7fffffffL));
    "op_i2f" >:: (fun _ -> run ~cmp:compare_f32 "op_i2f" "()F"
                     (Float (Float32.of_int32_bits 0x4f000000l)));
    "op_i2d" >:: (fun _ -> run "op_i2d" "()D" (Double (float_of_string "0x1.fffffffcp30")));

    "op_l2i" >:: (fun _ -> run "op_l2i" "()I" (Int (-1l)));
    "op_l2f" >:: (fun _ -> run ~cmp:compare_f32 "op_l2f" "()F"
                     (Float (Float32.of_int32_bits 0x5f000000l)));
    "op_l2d" >:: (fun _ -> run "op_l2d" "()D" (Double (float_of_string "0x1.0p63")));

    "op_f2i" >:: (fun _ -> run "op_f2i" "()I" (Int 314l));
    "op_f2l" >:: (fun _ -> run "op_f2l" "()J" (Long 314L));
    "op_f2d" >:: (fun _ -> run "op_f2d" "()D" (Double (float_of_string "0x1.3a28c6p8")));

    "op_f2i_nan" >:: (fun _ -> run "op_f2i_nan" "()I" (Int 0l));
    "op_f2l_nan" >:: (fun _ -> run "op_f2l_nan" "()J" (Long 0L));
    "op_f2d_nan" >:: (fun _ -> run ~cmp:compare_double_nan
                         "op_f2d_nan" "()D" (Double (float_of_string "nan")));

    "op_f2i_inf_pos" >:: (fun _ -> run "op_f2i_inf_pos" "()I" (Int 0x7fffffffl));
    "op_f2l_inf_pos" >:: (fun _ -> run "op_f2l_inf_pos" "()J" (Long 0x7fffffffffffffffL));
    "op_f2d_inf_pos" >:: (fun _ -> run "op_f2d_inf_pos" "()D" (Double (float_of_string "inf")));

    "op_f2i_inf_neg" >:: (fun _ -> run "op_f2i_inf_neg" "()I" (Int (0x80000000l)));
    "op_f2l_inf_neg" >:: (fun _ -> run "op_f2l_inf_neg" "()J" (Long (0x8000000000000000L)));
    "op_f2d_inf_neg" >:: (fun _ -> run "op_f2d_inf_neg" "()D" (Double (float_of_string "-inf")));

    "op_d2i" >:: (fun _ -> run "op_d2i" "()I" (Int 314l));
    "op_d2l" >:: (fun _ -> run "op_d2l" "()J" (Long 314L));
    "op_d2f" >:: (fun _ -> run ~cmp:compare_f32
                     "op_d2f" "()F" (Float (Float32.of_int32_bits 0x439d1463l)));

    "op_d2i_nan" >:: (fun _ -> run "op_d2i_nan" "()I" (Int 0l));
    "op_d2l_nan" >:: (fun _ -> run "op_d2l_nan" "()J" (Long 0L));
    "op_d2f_nan" >:: (fun _ -> run ~cmp:compare_float_nan
                         "op_d2f_nan" "()F" (Float (Float32.nan)));

    "op_d2i_inf_pos" >:: (fun _ -> run "op_d2i_inf_pos" "()I" (Int 0x7fffffffl));
    "op_d2l_inf_pos" >:: (fun _ -> run "op_d2l_inf_pos" "()J" (Long 0x7fffffffffffffffL));
    "op_d2f_inf_pos" >:: (fun _ -> run ~cmp:compare_f32
                             "op_d2f_inf_pos" "()F" (Float Float32.positive_infinity));

    "op_d2i_inf_neg" >:: (fun _ -> run "op_d2i_inf_neg" "()I" (Int 0x80000000l));
    "op_d2l_inf_neg" >:: (fun _ -> run "op_d2l_inf_neg" "()J" (Long 0x8000000000000000L));
    "op_d2f_inf_neg" >:: (fun _ -> run ~cmp:compare_f32
                             "op_d2f_inf_neg" "()F" (Float Float32.negative_infinity));

    "op_i2b_pos" >:: (fun _ -> run "op_i2b_pos" "()B" (Int 0x7fl));
    "op_i2c_pos" >:: (fun _ -> run "op_i2c_pos" "()C" (Int 0x7fffl));
    "op_i2s_pos" >:: (fun _ -> run "op_i2s_pos" "()S" (Int 0x7fffl));

    "op_i2b_neg" >:: (fun _ -> run "op_i2b_neg" "()B" (Int (-1l)));
    "op_i2c_neg" >:: (fun _ -> run "op_i2c_neg" "()C" (Int 0xffffl));
    "op_i2s_neg" >:: (fun _ -> run "op_i2s_neg" "()S" (Int (-1l)));
  ]

let () =
  run_test_tt_main suite
