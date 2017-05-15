open VMError
open Core.Std
open BatIO
open BatIO.BigEndian

module Field = struct
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrField.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrField.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

module Method = struct
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrMethod.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrMethod.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

type t =
  { minor_version : int;
    major_version : int;
    constant_pool : Poolbc.t;
    access_flags  : int;
    this_class    : int;
    super_class   : int;
    interfaces    : int list;
    fields        : Field.t list;
    methods       : Method.t list;
    attributes    : Attribute.AttrClass.t list;
  }

let loaded_bytecodes = Hashtbl.create ~hashable:String.hashable ()

let jar_infiles = Hashtbl.create ~hashable:String.hashable ()

let parse input =
  let check_magic input =
    if not (read_ui16 input = 0xCAFE && read_ui16 input = 0xBABE) then
      raise (ClassFormatError "Invalid magic")
  in
  let check_end input =
    try
      let _ = read input in
      raise (ClassFormatError "Invalid file ending")
    with
    | No_more_input -> ()
  in
  check_magic input;
  let minor_version = read_i16 input in
  let major_version = read_i16 input in
  let pool = Poolbc.create input in
  let access_flags = read_ui16 input in
  let this_class = read_ui16 input in
  let super_class = read_ui16 input in
  let interfaces = List.init (read_ui16 input) ~f:(fun _ -> read_ui16 input) in
  let fields = List.init (read_ui16 input) ~f:(fun _ -> Field.parse input pool) in
  let methods = List.init (read_ui16 input) ~f:(fun _ -> Method.parse input pool) in
  let attributes = List.init (read_ui16 input) ~f:(fun _ ->
      Attribute.AttrClass.parse input pool) in
  check_end input;
  { minor_version; major_version; constant_pool = pool;
    access_flags; this_class; super_class;
    interfaces; fields; methods ; attributes
  }

let load_from_stream input =
  parse input

let load_from_file binary_name file =
  match Sys.file_exists file with
  | `Yes when (Filename.basename file = binary_name ^ ".class") ->
    let input = BatFile.open_in file in
    let bytecode = parse input in
    BatIO.close_in input; Some bytecode
  | _ -> None

let load_from_jar binary_name filename =
  let load_from_infile in_file =
    try
      let entry = Zip.find_entry in_file (binary_name ^ ".class") in
      let content = Zip.read_entry in_file entry in
      let input = BatIO.input_string content in
      Some (parse input)
    with Not_found -> None
  in
  let infile = Hashtbl.find jar_infiles filename in
  match infile with
  | Some in_file -> load_from_infile in_file
  | _ -> let in_file = Zip.open_in filename in
    Hashtbl.add_exn jar_infiles ~key:filename ~data:in_file;
    load_from_infile in_file

(*
  CLASSPATH: bar.zip foo.jar dir dir/*
  http://docs.oracle.com/javase/8/docs/technotes/tools/unix/classpath.html#A1100592
*)

let rec find binary_name classpath =
  match classpath with
  | hd :: tail -> let bytecode = match Sys.is_directory hd with
      | `Yes ->
        let file = Filename.concat hd (binary_name ^ ".class") in
        load_from_file binary_name file
      | `No -> if Config.is_jar hd
        then load_from_jar binary_name hd else None
      | _ -> None
    in if Option.is_none bytecode then find binary_name tail else bytecode
  | [] -> None

let load binary_name =
  let bc = Hashtbl.find loaded_bytecodes binary_name in
  match bc with
  | Some code -> code
  | _ -> match find binary_name @@ Config.get_classpath () with
    | Some code -> Hashtbl.add_exn loaded_bytecodes ~key:binary_name ~data:code;code
    | None -> raise (ClassNotFoundException binary_name)
