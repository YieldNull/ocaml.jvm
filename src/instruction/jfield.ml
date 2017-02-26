open Accflag

include Classloader.InnField

let is_static jfield = FlagField.is_set jfield.access_flags FlagField.Static
