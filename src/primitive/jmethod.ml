open Accflag

include Types.InnMethod

let create jclass mid access_flags attrs =
  { jclass; mid; access_flags; attrs }

let jclass jmethod = jmethod.jclass

let mid jmethod = jmethod.mid

let name jmethod = MemberID.name jmethod.mid

let descriptor jmethod = MemberID.descriptor jmethod.mid

let access_flags jmethod = jmethod.access_flags

let attrs jmethod = jmethod.attrs

let is_static jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Static

let is_public jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Public

let is_protected jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Protected

let is_private jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Private

let is_abstract jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Abstract

let is_native jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Native

let equal m1 m2 =
  Jclass.equal m1.jclass m2.jclass && m1.mid = m2.mid
