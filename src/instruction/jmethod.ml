open Accflag

include Classloader.InnMethod

let memid jmethod = jmethod.mid

let jclass jmethod = jmethod.jclass

let name jmethod = jmethod.mid.MemberID.name

let descriptor jmethod = jmethod.mid.MemberID.descriptor

let attrs jmethod = jmethod.attrs

let is_protected jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Protected

let is_static jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Static

let is_abstract jmethod = FlagMethod.is_set jmethod.access_flags FlagMethod.Abstract
