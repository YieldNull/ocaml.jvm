open VMError
open Core.Std
open BatIO
open BatIO.BigEndian

module Field = struct
  type t =
    { name_index          : int;
      access_flags        : Accflag.t list;
      descriptor_index    : int;
      attributes          : Attribute.AttrField.t list;
    }

  let parse input pool =
    let access_flags = Accflag.parse (read_ui16 input) in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrField.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

module Method = struct
  type t =
    { name_index          : int;
      access_flags        : Accflag.t list;
      descriptor_index    : int;
      attributes          : Attribute.AttrMethod.t list;
    }

  let parse input pool =
    let access_flags = Accflag.parse (read_ui16 input) in
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
    access_flags  : Accflag.t list;
    this_class    : int;
    super_class   : int;
    interfaces    : int list;
    fields        : Field.t list;
    methods       : Method.t list;
    attributes    : Attribute.AttrClass.t list;
  }

let runtime = Hashtbl.create ~hashable:String.hashable ()
let cached = Hashtbl.create ~hashable:String.hashable ()

let rec cache_jar jar =
  let file = Zip.open_in jar in
  let entries = Zip.entries file in
  List.iter entries ~f:(fun entry ->
      if Filename.check_suffix entry.Zip.filename ".class" then
        let content = Zip.read_entry file entry in
        let input = BatIO.input_string content in
        let bytecode = load_from_stream input in
        let key = String.chop_suffix_exn entry.Zip.filename ~suffix:".class" in
        Hashtbl.add_exn runtime ~key:key ~data:bytecode;
        BatIO.close_in input
    );
  Zip.close_in file;
  Hashtbl.add_exn cached ~key:jar ~data:true

and load_from_stream input =
  parse input

and parse input =
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
  let access_flags = Accflag.parse (read_ui16 input) in
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

let load_from_jar jar_file path =
  let jar = Zip.open_in jar_file in
  let bytecode =
    try
      let entry = Zip.find_entry jar (path ^ ".class") in
      let content = Zip.read_entry jar entry in
      let input = BatIO.input_string content in
      Some (parse input)
    with Not_found -> None
  in
  Zip.close_in jar; bytecode

let load_from_dir dir path =
  let file = Filename.concat dir path in
  match Sys.file_exists file with
  | `Yes -> let input = BatFile.open_in file in
    let bytecode = parse input in
    BatIO.close_in input; Some bytecode
  | _ -> None

let load binary_name =
  let path = String.tr binary_name ~target:'.' ~replacement:'/' in
  let rec find classpath =
    match classpath with
    | hd :: tail -> let bytecode = match Sys.is_directory hd with
        | `Yes -> load_from_dir hd path
        | `No -> load_from_jar hd path
        | _ -> find tail
      in if Option.is_none bytecode then find tail else bytecode
    | [] -> None
  in
  let bc = Hashtbl.find runtime binary_name in
  match bc with
  | Some code -> code
  | _ -> match find @@ Config.get_classpath () with
    | Some code -> code
    | None -> raise (ClassNotFoundException binary_name)
