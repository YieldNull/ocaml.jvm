open Core.Std

type jclass =
  { name : string;
    access_flags : Accflag.t list;
    super_class  : jclass option;
    interfaces   : jclass list;
    fields : jfield list;
    methods : jmethod list;
    conspool : conspool array;
    attributes : Attribute.AttrClass.t list;
    loader : string;
  }
and jfield =
  { field_name          : string;
    field_descriptor    : string;
    field_access_flags  : Accflag.t list;
    field_attrs         : Attribute.AttrField.t list;
  }
and jmethod =
  { method_name          : string;
    method_descriptor    : string;
    method_access_flags  : Accflag.t list;
    method_attrs         : Attribute.AttrMethod.t list;
  }
and conspool =
  | Utf8 of string
  | Integer of int32
  | Float of float
  | Long of int64
  | Double of float
  | Class of jclass
  | String of string
  | Fieldref of jclass * jfield
  | Methodref of jclass * jmethod
  | InterfaceMethodref of string
  | NameAndType of int * int
  | MethodHandle of string
  | MethodType of string
  | InvokeDynamic of string
  | Byte8Placeholder

type t = jclass


let package name =
  let dot = String.rfindi name ~f:(fun _ c -> c = '.') in
  match dot with
  | Some i -> String.sub name ~pos:0 ~len:i
  | _ -> "."

  (* let rec resolute_class loader_name target_name =
    let jclass = match target_name with
      | '['::tail -> resolute_class loader_name tail
      | 'L'::tail -> load_class loader (String.of_char_list tail)
      | cls -> load_class loader (String.of_char_list cls)
    in
    if not (List.exists jclass.access_flags ~f:(fun flag -> flag = Accflag.Public))
    then
      raise (Class_format_error "")
    else
      jclass

  let resolute_conspool pool target =
    Array.iteri pool ~f:(fun i ele ->
        let entry = match ele with
          | Poolbc.Class index -> let name = Poolbc.get_class pool index in
            Class (load_class loader name)
        in target.(i) <- entry
      ) *)
