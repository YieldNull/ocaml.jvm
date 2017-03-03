open Core.Std

let () =
  let open Poolbc in
  let bytecode = Bytecode.load Sys.argv.(1) in
  Array.iteri bytecode.Bytecode.constant_pool ~f:(fun index entry ->
      let msg = match entry with
        | Utf8 str -> "Utf8: " ^ str
        | Integer i -> "Integer: " ^ Int32.to_string i
        | Float f -> "Float: " ^ string_of_float (Float32.to_double f)
        | Long l -> "Long: " ^ Int64.to_string l
        | Double d -> "Double: " ^ string_of_float d
        | Class i -> "Class: " ^ string_of_int i
        | String i -> "String: " ^ string_of_int i
        | Fieldref (ci, nti) -> sprintf "Fieldref: ci:%d nti:%d" ci nti
        | Methodref (ci, nti) -> sprintf "Methodref: ci:%d nti:%d" ci nti
        | InterfaceMethodref (ci, nti) -> sprintf "InterfaceMethodref: ci:%d nti:%d" ci nti
        | NameAndType (ni, di) -> sprintf "NameAndType: ni:%d di:%d" ni di
        | MethodHandle _ -> "MethodHandle"
        | MethodType _ -> "MethodType"
        | InvokeDynamic _ -> "InvokeDynamic"
        | Byte8Placeholder -> "~Byte8Placeholder"
      in
      printf "(%d) %s\n" (index + 1)  msg
    )
