open Core.Std
open OUnit2
open Jvalue
open TestEnv

let class_file =
  let file = "test/java/opcode/TestOpConstants.java" in
  compile file

let jclass =
  load_class class_file

let run ?(cmp = (=)) method_name method_descriptor result =
  let actual = run_method jclass method_name method_descriptor in
  assert_equal ~cmp actual (Some result)

let suite =
  "op_constants" >:::
  [ "op_aconst_null" >:: (fun _ -> run "op_aconst_null" "()Ljava/lang/Object;" Null);
    "op_bipush" >:: (fun _ -> run "op_bipush" "()B" (Int 127l));
    "op_sipush" >:: (fun _ -> run "op_sipush" "()S" (Int 32767l));

    "op_iconst_m1" >:: (fun _ -> run "op_iconst_m1" "()I" (Int (-1l)));
    "op_iconst_0" >:: (fun _ -> run "op_iconst_0" "()I" (Int 0l));
    "op_iconst_1" >:: (fun _ -> run "op_iconst_1" "()I" (Int 1l));
    "op_iconst_2" >:: (fun _ -> run "op_iconst_2" "()I" (Int 2l));
    "op_iconst_3" >:: (fun _ -> run "op_iconst_3" "()I" (Int 3l));
    "op_iconst_4" >:: (fun _ -> run "op_iconst_4" "()I" (Int 4l));
    "op_iconst_4" >:: (fun _ -> run "op_iconst_5" "()I" (Int 5l));

    "op_fconst_0" >:: (fun _ -> run ~cmp:compare_f32
                          "op_fconst_0" "()F" (Float (Float32.of_int32 0l)));
    "op_fconst_1" >:: (fun _ -> run ~cmp:compare_f32
                          "op_fconst_1" "()F" (Float (Float32.of_int32 1l)));
    "op_fconst_2" >:: (fun _ -> run ~cmp:compare_f32
                          "op_fconst_2" "()F" (Float (Float32.of_int32 2l)));

    "op_lconst_0" >:: (fun _ -> run "op_lconst_0" "()J" (Long 0L));
    "op_lconst_1" >:: (fun _ -> run "op_lconst_1" "()J" (Long 1L));
    "op_dconst_0" >:: (fun _ -> run "op_dconst_0" "()D" (Double 0.0));
    "op_dconst_1" >:: (fun _ -> run "op_dconst_1" "()D" (Double 1.0));

    "op_ldc_int" >:: (fun _ -> run "op_ldc_int" "()I" (Int 2147483647l));
    "op_ldc_float" >:: (fun _ -> run ~cmp:compare_f32 "op_ldc_float" "()F"
                           (Float (Float32.of_int32 3l)));
    "op_ldc_string" >:: (fun _ ->
        let strobj =
          (Jstring.find_or_create (Unicode.uchar_arr_to_modified_utf8
                                     [| Jvalue.Char 0x4f60; Jvalue.Char 0x597d|])) in
        run ~cmp:compare_string "op_ldc_string" "()Ljava/lang/String;" (Object strobj)
      );

    "op_ldc_w_int" >:: (fun _ -> run "op_ldc_w_int" "()I" (Int 2147483646l));
    "op_ldc_w_float" >:: (fun _ ->
        run ~cmp:compare_f32 "op_ldc_w_float" "()F" (Float (Float32.of_int32 9l)));
    "op_ldc_w_string" >:: (fun _ ->
        let strobj = (Jstring.find_or_create (Unicode.uchar_arr_to_modified_utf8
                                                [| Jvalue.Char 0x4e16; Jvalue.Char 0x754c|])) in
        run ~cmp:compare_string "op_ldc_w_string" "()Ljava/lang/String;" (Object strobj)
      );
    "op_ldc2_w_long" >:: (fun _ -> run "op_ldc2_w_long" "()J" (Long 3L));
    "op_ldc2_w_double" >:: (fun _ -> run "op_ldc2_w_double" "()D" (Double 3.0));
  ]

let () =
  run_test_tt_main suite;
  delete class_file
