module Field : sig
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrField.t list;
    }
end

module Method : sig
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrMethod.t list;
    }
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

val load : string -> t
val load_from_stream : BatInnerIO.input -> t
