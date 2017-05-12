open Core.Std
open VMError

let modified_utf8_to_unicode str =
  let bytes_count = String.length str in
  let length = ref 0 in
  let counter = ref 0 in
  let buffer = Array.create ~len:bytes_count 0 in
  let read_byte () =
    let byte = String.get str !counter in
    counter := 1 + !counter;
    Char.to_int byte
  in
  let byte_one x = (* 0xxxxxxx *)
    x
  in
  let byte_two x = (* 110x xxxx   10xx xxxx *)
    let y = read_byte () in
    if y lsr 6 <> 2 then raise VirtualMachineError
    else ((x land 0x1F) lsl 6) + (y land 0x3F)
  in
  let byte_three x = (* 1110 xxxx  10xx xxxx  10xx xxxx *)
    let y = read_byte () in
    let z = read_byte () in
    if y lsr 6 <> 2 then raise VirtualMachineError;
    if z lsr 6 <> 2 then raise VirtualMachineError;
    ((x land 0xF) lsl 12) + ((y land 0x3F) lsl 6) + (z land 0x3F)
  in
  while !counter < bytes_count do
    let byte = read_byte () in
    let head = byte lsr 4 in
    let chr = match head with
      | x when x < 8 -> byte_one byte
      | (12 | 13)  -> byte_two byte
      | 14 -> byte_three byte
      | _ -> raise VirtualMachineError
    in
    length := 1 + !length;
    Array.set buffer (!length - 1) chr
  done;
  List.init !length ~f:(fun i -> Array.get buffer i)
