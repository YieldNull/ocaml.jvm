open Accflag

include Classloader.InnMethod

let memid jmethod = jmethod.mid

let jclass jmethod = jmethod.jclass

let name jmethod = jmethod.mid.MemberID.name

let descriptor jmethod = jmethod.mid.MemberID.descriptor

let attrs jmethod = jmethod.attrs

let access_flags jmethod = jmethod.access_flags

let is_protected jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Protected

let is_static jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Static

let is_abstract jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Abstract

let is_private jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Private

let is_public jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Public

let is_native jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Native

let equal m1 m2 =
  Jclass.equal m1.jclass m2.jclass && m1.mid = m2.mid
