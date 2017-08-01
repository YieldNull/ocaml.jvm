open Core

let test_mutf_to_unicode () =
  let chars = [| 0xED; 0xA0; 0x81; 0xED; 0xB0; 0x80 |] in (* 0xEDA0 81ED B080 *)
  let mutf8_str = String.init 6 ~f:(fun i -> Char.of_int_exn @@ Array.get chars i) in
  let unicode_str = Unicode.modified_utf8_to_unicode mutf8_str in (* \uD801 \uDC00 *)
  assert (String.get unicode_str 0 = Caml.Char.chr 0xD8);
  assert (String.get unicode_str 1 = Caml.Char.chr 0x01);
  assert (String.get unicode_str 2 = Caml.Char.chr 0xDC);
  assert (String.get unicode_str 3 = Caml.Char.chr 0x00)

let test_mutf_to_uchar_arr () =
  let chars = [| 0xED; 0xA0; 0x81; 0xED; 0xB0; 0x80 |] in (* 0xEDA0 81ED B080 *)
  let mutf8_str = String.init 6 ~f:(fun i -> Char.of_int_exn @@ Array.get chars i) in
  let char_arr = Unicode.modified_utf8_to_uchar_arr mutf8_str in (* 0xD801 0xDC00 *)
  assert (char_arr.(0) = Jvalue.Char 0xD801);
  assert (char_arr.(1) = Jvalue.Char 0xDC00)


let test_uchar_arr_to_mutf () =
  let uchars = [| Jvalue.Char 0xD801; Jvalue.Char 0xDC00 |] in
  let mutf = Unicode.uchar_arr_to_modified_utf8 uchars in (* 0xEDA0 81ED B080 *)
  assert (String.get mutf 0 = Caml.Char.chr 0xED);
  assert (String.get mutf 1 = Caml.Char.chr 0xA0);
  assert (String.get mutf 2 = Caml.Char.chr 0x81);
  assert (String.get mutf 3 = Caml.Char.chr 0xED);
  assert (String.get mutf 4 = Caml.Char.chr 0xB0);
  assert (String.get mutf 5 = Caml.Char.chr 0x80)

let () =
  test_mutf_to_unicode ();
  test_mutf_to_uchar_arr ();
  test_uchar_arr_to_mutf ()
