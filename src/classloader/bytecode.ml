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

let runtime = Hashtbl.create ~hashable:String.hashable ()

let rec cache_jar input =
  let entries = Zip.entries input in
  List.iter entries ~f:(fun entry ->
      if Filename.check_suffix entry.Zip.filename ".class" then
        let content = Zip.read_entry input entry in
        let input = BatIO.input_string content in
        let bytecode = load_from_stream input in
        let key = String.chop_suffix_exn entry.Zip.filename ~suffix:".class" in
        Hashtbl.add_exn runtime ~key:key ~data:bytecode;
        BatIO.close_in input
    );
  Zip.close_in input;

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

let load binary_name =
  let load_from_file file =
    match Sys.file_exists file with
    | `Yes when (Filename.basename file = binary_name ^ ".class") ->
      let input = BatFile.open_in file in
      let bytecode = parse input in
      BatIO.close_in input; Some bytecode
    | _ -> None
  in
  let load_from_jar jar =
    let jar_stream = Zip.open_in jar in
    let bytecode =
      try
        let entry = Zip.find_entry jar_stream (binary_name ^ ".class") in
        let content = Zip.read_entry jar_stream entry in
        let input = BatIO.input_string content in
        cache_jar jar_stream; (* load all bytecodes in the same jar file *)
        Some (parse input)
      with Not_found -> None
    in
    Zip.close_in jar_stream; bytecode
  in
  let rec find classpath =
    match classpath with
    | hd :: tail -> let bytecode = match Sys.is_directory hd with
        | `Yes -> load_from_dir hd
        | `No -> if String.is_suffix hd ~suffix:".jar"
        (* && not @@ String.is_prefix hd ~prefix:"." *)
          then load_from_jar hd
          else if String.is_suffix hd ~suffix:".class"
          then load_from_file hd
          else None
        | _ -> None
      in if Option.is_none bytecode then find tail else bytecode
    | [] -> None
  and load_from_dir dir =
    let file = Filename.concat dir (binary_name ^ ".class") in
    match load_from_file file with
    | Some code -> Some code
    | _ -> let dirs, files =
             Sys.readdir dir
             |> Array.to_list
             |> List.map ~f:(Filename.concat dir)
             |> List.split_while
               ~f:(fun file -> Sys.is_directory file = `Yes
                               && not @@ String.is_prefix file ~prefix:".")
      in find (files @ dirs)
  in
  let bc = Hashtbl.find runtime binary_name in
  match bc with
  | Some code -> code
  | _ -> match find @@ Config.get_classpath () with
    | Some code -> code
    | None -> raise (ClassNotFoundException binary_name)
