open Core.Std
open OUnit2
open Jvalue
open TestEnv

let jclass = compile_jclass "test/java/opcode/TestOpStoreAndLoad.java"

let run ?(cmp = (=)) method_name method_descriptor result =
  let actual = run_method jclass method_name method_descriptor in
  assert_equal ~cmp actual (Some result)

let suite =
  "op_store_and_load" >:::
  [ "op_istore_0_iload_0" >:: (fun _ -> run "op_istore_0_iload_0" "()I" (Int (-1l)));
    "op_istore_1_iload_1" >:: (fun _ -> run "op_istore_1_iload_1" "()I" (Int (-1l)));
    "op_istore_2_iload_2" >:: (fun _ -> run "op_istore_2_iload_2" "()I" (Int (-1l)));
    "op_istore_3_iload_3" >:: (fun _ -> run "op_istore_3_iload_3" "()I" (Int (-1l)));
    "op_istore_iload" >:: (fun _ -> run "op_istore_iload" "()I" (Int (-1l)));

    "op_fstore_0_fload_0" >:: (fun _ ->
        run ~cmp:compare_f32 "op_fstore_0_fload_0" "()F" (Float (Float32.of_int32 (-1l))));
    "op_fstore_1_fload_1" >:: (fun _ ->
        run ~cmp:compare_f32 "op_fstore_1_fload_1" "()F" (Float (Float32.of_int32 (-1l))));
    "op_fstore_2_fload_2" >:: (fun _ ->
        run ~cmp:compare_f32 "op_fstore_2_fload_2" "()F" (Float (Float32.of_int32 (-1l))));
    "op_fstore_3_fload_3" >:: (fun _ ->
        run ~cmp:compare_f32 "op_fstore_3_fload_3" "()F" (Float (Float32.of_int32 (-1l))));
    "op_fstore_fload" >:: (fun _ ->
        run ~cmp:compare_f32 "op_fstore_fload" "()F" (Float (Float32.of_int32 (-1l))));

    "op_lstore_0_lload_0" >:: (fun _ -> run "op_lstore_0_lload_0" "()J" (Long (-1L)));
    "op_lstore_1_lload_1" >:: (fun _ -> run "op_lstore_1_lload_1" "()J" (Long (-1L)));
    "op_lstore_2_lload_2" >:: (fun _ -> run "op_lstore_2_lload_2" "()J" (Long (-1L)));
    "op_lstore_3_lload_3" >:: (fun _ -> run "op_lstore_3_lload_3" "()J" (Long (-1L)));
    "op_lstore_lload" >:: (fun _ -> run "op_lstore_lload" "()J" (Long (-1L)));

    "op_dstore_0_dload_0" >:: (fun _ -> run "op_dstore_0_dload_0" "()D" (Double (-1.0)));
    "op_dstore_1_dload_1" >:: (fun _ -> run "op_dstore_1_dload_1" "()D" (Double (-1.0)));
    "op_dstore_2_dload_2" >:: (fun _ -> run "op_dstore_2_dload_2" "()D" (Double (-1.0)));
    "op_dstore_3_dload_3" >:: (fun _ -> run "op_dstore_3_dload_3" "()D" (Double (-1.0)));
    "op_dstore_dload" >:: (fun _ -> run "op_dstore_dload" "()D" (Double (-1.0)));

    "op_astore_0_aload_0" >:: (fun _ -> run "op_astore_0_aload_0" "()Ljava/lang/Object;" Null);
    "op_astore_1_aload_1" >:: (fun _ -> run "op_astore_1_aload_1" "()Ljava/lang/Object;" Null);
    "op_astore_2_aload_2" >:: (fun _ -> run "op_astore_2_aload_2" "()Ljava/lang/Object;" Null);
    "op_astore_3_aload_3" >:: (fun _ -> run "op_astore_3_aload_3" "()Ljava/lang/Object;" Null);
    "op_astore_aload" >:: (fun _ -> run "op_astore_aload" "()Ljava/lang/Object;" Null);

    "op_bastore_baload" >:: (fun _ -> run "op_bastore_baload" "()B" (Int (-1l)));
    "op_sastore_saload" >:: (fun _ -> run "op_sastore_saload" "()S" (Int (-1l)));
    "op_castore_caload" >:: (fun _ -> run "op_castore_caload" "()C" (Int 65535l));
    "op_iastore_iaload" >:: (fun _ -> run "op_iastore_iaload" "()I" (Int (-1l)));
    "op_fastore_faload" >:: (fun _ ->
        run ~cmp:compare_f32 "op_fastore_faload" "()F" (Float (Float32.of_int32 (-1l))));
    "op_lastore_laload" >:: (fun _ -> run "op_lastore_laload" "()J" (Long (-1L)));
    "op_dastore_daload" >:: (fun _ -> run "op_dastore_daload" "()D" (Double (-1.0)));
    "op_aastore_aaload" >:: (fun _ -> run "op_aastore_aaload" "()Ljava/lang/Object;" Null);
  ]

let () =
  run_test_tt_main suite
