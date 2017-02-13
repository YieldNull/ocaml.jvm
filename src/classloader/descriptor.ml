open Core.Std

type field =
  | Byte
  | Short
  | Char
  | Int
  | Float
  | Long
  | Double
  | Boolean
  | Class of string

let type_of_field descriptor =
  match String.get descriptor 0 with
  | 'B' -> Byte
  | 'S' -> Short
  | 'C' -> Char
  | 'I' -> Int
  | 'F' -> Float
  | 'J' -> Long
  | 'D' -> Double
  | 'Z' -> Boolean
  | 'L' -> Class (String.sub descriptor ~pos:1 ~len:(String.length descriptor - 2))
  | _ -> Class descriptor (* array *)

let classes_of_method descriptor =
  let rec parse_class lst =
    match lst with
    | ';' :: tail -> [], tail
    | chr :: tail -> let cls, rest = parse_class tail in chr :: cls, rest
    | [] -> [], []
  in
  let rec parse_array lst =
    let comp, last = match lst with
      | '[' :: tail -> parse_array tail
      | 'L' :: tail -> let cls, rest = parse_class tail in
        sprintf "L%s;" (String.of_char_list cls), rest
      | chr :: tail -> String.of_char chr, tail
      | [] -> failwith ""
    in "[" ^ comp, last
  in
  let rec parse = function
    | [] -> []
    | ('(' | 'B' | 'S' | 'C' | 'I' | 'F' | 'J' | 'D' | 'Z' | ')' ) :: tail -> parse tail
    | '[' :: tail -> let cls, rest = parse_array tail in cls :: (parse rest)
    | _ :: tail -> let chrs, rest = parse_class tail
      in (String.of_char_list chrs) :: (parse rest)
  in parse (String.to_list descriptor)
