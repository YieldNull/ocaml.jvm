open Core.Std
open OUnit2
open Jvalue
open TestEnv

let jclass = compile_jclass "test/java/opcode/TestOpComparison.java"

let run ?(cmp = (=)) method_name method_descriptor args result =
  let actual = run_method_with_args jclass method_name method_descriptor args in
  assert_equal ~cmp actual (Some result)

let suite =
  "op_comparison" >:::
  [ "op_if_icmpeq" >:: (fun _ -> run "op_if_icmpeq" "(II)I" [| Int 0l; Int 1l |] (Int 0l));
    "op_if_icmpne" >:: (fun _ -> run "op_if_icmpne" "(II)I" [| Int 2l; Int 1l |] (Int 1024l));
    "op_if_icmplt" >:: (fun _ -> run "op_if_icmplt" "(II)I" [| Int 0l; Int 1l |] (Int 1024l));
    "op_if_icmpgt" >:: (fun _ -> run "op_if_icmpgt" "(II)I" [| Int 0l; Int 1l |] (Int 0l));
    "op_if_icmpge_gt" >:: (fun _ -> run "op_if_icmpge" "(II)I" [| Int 2l; Int 1l |] (Int 1024l));
    "op_if_icmpge_eq" >:: (fun _ -> run "op_if_icmpge" "(II)I" [| Int 1l; Int 1l |] (Int 1024l));
    "op_if_icmple_lt" >:: (fun _ -> run "op_if_icmple" "(II)I" [| Int 0l; Int 1l |] (Int 1024l));
    "op_if_icmple_eq" >:: (fun _ -> run "op_if_icmple" "(II)I" [| Int 1l; Int 1l |] (Int 1024l));

    "op_ifeq" >:: (fun _ -> run "op_ifeq" "(I)I" [| Int 1l |] (Int 0l));
    "op_ifne" >:: (fun _ -> run "op_ifne" "(I)I" [| Int 2l |] (Int 1024l));
    "op_iflt" >:: (fun _ -> run "op_iflt" "(I)I" [| Int (-1l) |] (Int 1024l));
    "op_ifgt" >:: (fun _ -> run "op_ifgt" "(I)I" [| Int 0l |] (Int 0l));
    "op_ifge_gt" >:: (fun _ -> run "op_ifge" "(I)I" [| Int 2l |] (Int 1024l));
    "op_ifge_eq" >:: (fun _ -> run "op_ifge" "(I)I" [| Int 0l |] (Int 1024l));
    "op_ifle_lt" >:: (fun _ -> run "op_ifle" "(I)I" [| Int (-1l) |] (Int 1024l));
    "op_ifle_eq" >:: (fun _ -> run "op_ifle" "(I)I" [| Int 0l |] (Int 1024l));

    "op_lcmp" >:: (fun _ -> run "op_lcmp" "(JJ)I" [| Long 2L; Null; Long 2L|] (Int 1024l));

    "op_fcmpg" >:: (fun _ -> run "op_fcmpg" "(FF)I"
                       [| Float (Float32.of_int32 2l); Float (Float32.of_int32 1l)|] (Int 1024l));
    "op_fcmpg_nan" >:: (fun _ -> run "op_fcmpg" "(FF)I"
                           [| Float (Float32.of_int32 2l); Float Float32.nan|] (Int 1024l));

    "op_fcmpl" >:: (fun _ -> run "op_fcmpl" "(FF)I"
                       [| Float (Float32.of_int32 2l); Float (Float32.of_int32 1l)|] (Int 0l));
    "op_fcmpl_nan" >:: (fun _ -> run "op_fcmpl" "(FF)I"
                           [| Float (Float32.of_int32 2l); Float Float32.nan|] (Int 1024l));

    "op_if_acmpeq" >:: (fun _ ->
        let s = Jstring.find_or_create "Hello" in
        let ss = Jstring.find_or_create "Hello" in
        run "op_if_acmpeq" "(Ljava/lang/String;Ljava/lang/String;)Z"
          [| Object s; Object ss |] (Int 0l));

    "op_if_acmpne" >:: (fun _ ->
        let s = Jstring.find_or_create "Hello" in
        run "op_if_acmpne" "(Ljava/lang/String;Ljava/lang/String;)Z"
          [| Object s; Null |] (Int 0l));
  ]

let () =
  run_test_tt_main suite
