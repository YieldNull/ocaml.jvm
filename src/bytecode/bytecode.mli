module Field : sig
  type t =
    { mid                 : MemberID.t;
      access_flags        : int;
      attributes          : Attribute.AttrField.t list;
    }
end

module Method : sig
  type t =
    { mid                 : MemberID.t;
      access_flags        : int;
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
    attributes    : Attribute.AttrClass.t list;
    fields        : Field.t list;
    static_fields : Field.t list;
    static_methods  : Method.t list;
    virtual_methods : Method.t list;
    special_methods : Method.t list;
  }

val load : string -> t
val load_from_stream : BatInnerIO.input -> t
