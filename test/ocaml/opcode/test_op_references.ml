open Core.Std
open OUnit2
open Jvalue
open TestEnv

let jclass = compile_jclass "test/java/opcode/TestOpReferences.java"

let run ?(cmp = (=)) method_name method_descriptor result =
  let actual = run_method jclass method_name method_descriptor in
  assert_equal ~cmp actual (Some result)

let suite =
  "op_references" >:::
  [ "op_getstatic" >:: (fun _ -> run "op_getstatic" "()I" (Int 0l));
    "op_putstatic" >:: (fun _ -> run "op_putstatic" "()I" (Int 2l));
    "op_invokestatic" >:: (fun _ -> run "op_invokestatic" "()J" (Long 699359257085L));
  ]

let () =
  run_test_tt_main suite
