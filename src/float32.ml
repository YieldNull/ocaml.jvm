type float32
type t = float32

external atom_zero : unit -> float32 = "float32_zero"
external atom_one : unit -> float32 = "float32_one"

external add : float32 -> float32 -> float32 = "float32_add"
external sub : float32 -> float32 -> float32 = "float32_sub"
external mul : float32 -> float32 -> float32 = "float32_mul"
external div : float32 -> float32 -> float32 = "float32_div"
external rem : float32 -> float32 -> float32 = "float32_rem"
external equal : float32 -> float32 -> bool = "float32_equal"
external neg : float32 -> float32 = "float32_neg"

external is_nan : float32 -> bool = "float32_is_nan"
external compare : float32 -> float32 -> int32 = "float32_compare"

external to_int32 : float32 -> int32 = "float32_to_int32"
external to_int64 : float32 -> int64 = "float32_to_int64"
external to_float64 : float32 -> float = "float32_to_float64"


external bits_of_int32 : int32 -> float32 = "float32_bits_of_int32"
external of_int32 : int32 -> float32 = "float32_of_int32"
external of_int64 : int64 -> float32 = "float32_of_int64"
external of_float64 : float -> float32 = "float32_of_float64"

let zero = atom_zero ()
let one = atom_one ()