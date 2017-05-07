open Core.Std

let op_iconst_i jclass i =
  let result = Test_opcode_env.run_method jclass (sprintf "op_iconst_%d" i) "()I" in
  assert (result = Some (Jvalue.Int (Int32.of_int_exn i)))


let () =
  let file = "test/java/opcode/TestOpConstants.java" in
  let class_file = Test_opcode_env.compile file in
  let jclass = Test_opcode_env.load_class class_file in
  List.iter (List.range 0 6) ~f:(op_iconst_i jclass)
