open Core.Std

include Classloader.InnClass

let is_array jclass = Array.is_empty jclass.conspool

let conspool jclass = jclass.conspool
