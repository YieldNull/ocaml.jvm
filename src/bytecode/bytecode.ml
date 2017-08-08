open VMError
open Core
open BatIO
open BatIO.BigEndian
open Accflag

(*
  When a jar file is first loaded, cache all the packages in it.

  When searching a class in classpath,
    find the jar files where the class may be defined by its package.
  If a jar file is cached but not in the jar files, just skip unziping it.
*)

module Field = struct
  type t =
    { mid                 : MemberID.t;
      access_flags        : int;
      attributes          : Attribute.AttrField.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name = Poolbc.get_string pool (read_ui16 input) in
    let descriptor = Poolbc.get_string pool (read_ui16 input) in
    let mid = MemberID.create name descriptor in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrField.parse input pool) in
    { mid; access_flags; attributes}
end

module Method = struct
  type t =
    { mid                 : MemberID.t;
      access_flags        : int;
      attributes          : Attribute.AttrMethod.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name = Poolbc.get_string pool (read_ui16 input) in
    let descriptor = Poolbc.get_string pool (read_ui16 input) in
    let mid = MemberID.create name descriptor in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrMethod.parse input pool) in
    { mid; access_flags; attributes}
end

type t =
  { minor_version : int;
    major_version : int;
    constant_pool : Poolbc.t;
    access_flags  : int;
    this_class    : int;
    super_class   : int;
    interfaces    : int list;
    attributes    : Attribute.AttrClass.t list;
    fields        : Field.t list;
    static_fields : Field.t list;
    methods       : Method.t list;
  }

let read_fields input pool =
  let open Field in
  List.fold (List.range 0 (read_ui16 input)) ~init:([], [])
    ~f:(fun acc _ ->
        let f = parse input pool in
        let static, normal = acc in
        if FlagField.is_set f.access_flags FlagField.Static then
          f :: static, normal
        else
          static, f :: normal
      )

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
  let static_fields, fields = read_fields input pool in
  let methods = List.init (read_ui16 input) ~f:(fun _ -> Method.parse input pool) in
  let attributes = List.init (read_ui16 input) ~f:(fun _ ->
      Attribute.AttrClass.parse input pool) in
  check_end input;
  { minor_version; major_version; constant_pool = pool;
    access_flags; this_class; super_class;
    interfaces; attributes; fields; static_fields; methods;
  }

let loaded_bytecodes = Hashtbl.create ~hashable:String.hashable ()

let package_in_jar = Hashtbl.create ~hashable:String.hashable ()

let cached_jar = Hashtbl.create ~hashable:String.hashable ()

let cache_jar in_file =
  let filename = in_file.Jar.if_filename in
  List.iter (Jar.entries in_file) ~f:(fun entry ->
      if not entry.Jar.is_directory
      && Filename.check_suffix entry.Jar.filename ".class"
      then begin
        let package = Filename.dirname entry.Jar.filename in
        match Hashtbl.find package_in_jar package with
        | Some jarlist when Option.is_some @@ List.find jarlist ~f:((=) filename) -> ()
        | _ -> Hashtbl.add_multi package_in_jar ~key:package ~data:filename
      end
    );
  Hashtbl.add_exn cached_jar ~key:filename ~data:in_file

let load_from_infile binary_name in_file =
  try
    let entry = Jar.find_entry in_file (binary_name ^ ".class") in
    let content = Jar.read_entry in_file entry in
    let input = BatIO.input_string content in
    Some (parse input)
  with Not_found -> None

let rec load_related_class in_file bytecode =
  Array.iter bytecode.constant_pool ~f:(fun entry ->
      match entry with
      | Poolbc.Class index ->
        begin
          let binary_name = Poolbc.get_utf8 bytecode.constant_pool index in
          if Option.is_none @@ Hashtbl.find loaded_bytecodes binary_name then
            begin
              match load_from_infile binary_name in_file with
              | Some code ->
                ignore @@ Hashtbl.add loaded_bytecodes ~key:binary_name ~data:code;
                load_related_class in_file code;
              | _ -> ()
            end
        end
      | _ -> ()
    )

let load_from_jar binary_name filename =
  let aux () =
    let in_file = match Hashtbl.find cached_jar filename with
      | Some infile -> Jar.set_if_channel infile filename; infile
      | None -> let infile = Jar.open_in filename in
        cache_jar infile; infile
    in
    let code = load_from_infile binary_name in_file in
    (* begin
       match code with
       | Some bytecode -> load_related_class in_file bytecode
       | _ -> ()
       end; *)
    Jar.close_in in_file;
    code
  in
  match Sys.file_exists filename with
  | `Yes -> aux ()
  | _ -> None

let load_from_stream input =
  parse input

let load_from_file binary_name file =
  match Sys.file_exists file with
  | `Yes when (Filename.basename file = binary_name ^ ".class") ->
    let input = BatFile.open_in file in
    let bytecode = parse input in
    BatIO.close_in input; Some bytecode
  | _ -> None

(*
  CLASSPATH: bar.zip foo.jar dir dir/*
  http://docs.oracle.com/javase/8/docs/technotes/tools/unix/classpath.html#A1100592
*)

let find binary_name classpath =
  let package = Filename.dirname binary_name in
  let jar_files = Hashtbl.find package_in_jar package in
  let rec aux paths =
    match paths with
    | hd :: tail -> let bytecode = match Sys.is_directory hd with
        | `Yes ->
          let file = Filename.concat hd (binary_name ^ ".class") in
          load_from_file binary_name file
        | `No -> begin match jar_files with
            | Some jarlist
              when Option.is_none @@ List.find jarlist ~f:((=) hd)
                && Option.is_some @@ Hashtbl.find cached_jar hd (* jar is cached but not in jarlist *)
              -> None
            | _ -> load_from_jar binary_name hd
          end
        | _ -> None
      in if Option.is_none bytecode then aux tail else bytecode
    | [] -> None
  in aux classpath

let load binary_name =
  let bc = Hashtbl.find loaded_bytecodes binary_name in
  match bc with
  | Some code -> code
  | _ -> match find binary_name @@ Config.get_classpath () with
    | Some code -> ignore @@ Hashtbl.add loaded_bytecodes ~key:binary_name ~data:code;code
    | None -> raise (ClassNotFoundException binary_name)
