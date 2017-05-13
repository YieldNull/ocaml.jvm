open Core.Std
open VMError

let modified_utf8_to_codepoint str ~f =
  let counter = ref 0 in
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
  while !counter < String.length str do
    let byte = read_byte () in
    let head = byte lsr 4 in
    let codepoint = match head with
      | x when x < 8 -> byte_one byte
      | (12 | 13)  -> byte_two byte
      | 14 -> byte_three byte
      | _ -> raise VirtualMachineError
    in
    f codepoint
  done

let modified_utf8_to_unicode str =
  let bytes_count = String.length str in
  let length = ref 0 in
  let buffer = Array.create ~len:bytes_count 0 in
  modified_utf8_to_codepoint str ~f:(fun codepoint ->
      Array.set buffer !length codepoint;
      length := 1 + !length;
    );
  String.init (!length * 2) ~f:(fun i ->
      let codepoint = Array.get buffer (i / 2) in
      let chr =
        if i mod 2 = 0 then codepoint lsr 8 (* high *)
        else codepoint land 0xFF (* low *)
      in
      Caml.Char.chr chr
    )

let modified_utf8_to_uchar_arr str =
  let bytes_count = String.length str in
  let length = ref 0 in
  let buffer = Array.create ~len:bytes_count (Jvalue.Char 0) in
  modified_utf8_to_codepoint str ~f:(fun codepoint ->
      Array.set buffer !length (Jvalue.Char codepoint);
      length := 1 + !length;
    );
  Array.sub buffer ~pos:0 ~len:!length

let uchar_arr_to_modified_utf8 uchars =
  let value uchar =
    match uchar with
    | Jvalue.Char chr -> chr
    | _ -> assert false
  in
  let length = Array.fold uchars ~init:0 ~f:(fun acc jchar ->
      let uchar = value jchar in
      if uchar >= 0x0001 && uchar <= 0x007F then 1 + acc
      else if uchar >= 0x0080 && uchar <= 0x07FF then 2 + acc
      else 3 + acc
    )
  in
  let str = String.create length in
  let count = ref 0 in
  Array.iter uchars ~f:(fun jchar ->
      let uchar = value jchar in
      if uchar >= 0x0001 && uchar <= 0x007F then
        let chr = Caml.Char.chr uchar in
        String.set str !count chr;
        count := 1 + !count
      else if uchar >= 0x0080 && uchar <= 0x07FF then
        let chr = Caml.Char.chr (0xC0 lor (0x1F land (uchar lsr 6))) in
        let chr2 = Caml.Char.chr (0x80 lor (0x3F land uchar)) in
        String.set str !count chr;
        String.set str (!count + 1) chr2;
        count := !count + 2
      else
        let chr = Caml.Char.chr (0xE0 lor (0x0F land (uchar lsr 12))) in
        let chr2 = Caml.Char.chr (0x80 lor (0x3F land (uchar lsr 6))) in
        let chr3 = Caml.Char.chr (0x80 lor (0x3F land uchar))in
        String.set str !count chr;
        String.set str (!count + 1) chr2;
        String.set str (!count + 2) chr3;
        count := !count + 3
    );
  str
