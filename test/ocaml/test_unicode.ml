open Core.Std

let test_unicode () =
  let chars = [| 0xED; 0xA0; 0x81; 0xED; 0xB0; 0x80 |] in (* 0xEDA0 81ED B080 *)
  let mutf8_str = String.init 6 ~f:(fun i -> Char.of_int_exn @@ Array.get chars i) in
  let unicode_str = Unicode.modified_utf8_to_unicode mutf8_str in (* \uD801 \uDC00 *)
  assert (String.get unicode_str 0 = Caml.Char.chr 0xD8);
  assert (String.get unicode_str 1 = Caml.Char.chr 0x01);
  assert (String.get unicode_str 2 = Caml.Char.chr 0xDC);
  assert (String.get unicode_str 3 = Caml.Char.chr 0x00)

let test_uchar_arr () =
  let chars = [| 0xED; 0xA0; 0x81; 0xED; 0xB0; 0x80 |] in (* 0xEDA0 81ED B080 *)
  let mutf8_str = String.init 6 ~f:(fun i -> Char.of_int_exn @@ Array.get chars i) in
  let char_arr = Unicode.modified_utf8_to_uchar_arr mutf8_str in (* 0xD801 0xDC00 *)
  assert (char_arr.(0) = 0xD801);
  assert (char_arr.(1) = 0xDC00)

let () =
  test_unicode ();
  test_uchar_arr ()
