open Core
open VMError

type t =
  | Byte
  | Short
  | Char
  | Int
  | Float
  | Long
  | Double
  | Boolean
  | Class of string

let rec parse_class lst =
  match lst with
  | ';' :: tail -> [], tail
  | chr :: tail -> let cls, rest = parse_class tail in chr :: cls, rest
  | [] -> [], []

let rec parse_array lst =
  let comp, last = match lst with
    | '[' :: tail -> parse_array tail
    | 'L' :: tail -> let cls, rest = parse_class tail in
      sprintf "L%s;" (String.of_char_list cls), rest
    | chr :: tail -> String.of_char chr, tail
    | [] -> failwith ""
  in "[" ^ comp, last

let classes_of_method descriptor =
  let rec parse = function
    | [] -> []
    | ('(' | 'B' | 'S' | 'C' | 'I' | 'F' | 'J' | 'D' | 'Z' | ')' ) :: tail -> parse tail
    | '[' :: tail -> let cls, rest = parse_array tail in cls :: (parse rest)
    | _ :: tail -> let chrs, rest = parse_class tail
      in (String.of_char_list chrs) :: (parse rest)
  in parse (String.to_list descriptor)

let args_of_method descriptor =
  let rec parse_class = function
    | ';' :: tail -> [], tail
    | chr :: tail -> let cls, rest = parse_class tail in chr :: cls, rest
    | [] -> [], []
  in
  let rec parse = function
    | '(' :: tail -> parse tail
    | 'B' :: tail -> let c,lst = parse tail in c, Byte :: lst
    | 'S' :: tail -> let c,lst = parse tail in c, Short :: lst
    | 'C' :: tail -> let c,lst = parse tail in c, Char :: lst
    | 'I' :: tail -> let c,lst = parse tail in c, Int :: lst
    | 'F' :: tail -> let c,lst = parse tail in c, Float :: lst
    | 'J' :: tail -> let c,lst = parse tail in c + 1, Long :: lst
    | 'D' :: tail -> let c,lst = parse tail in c + 1, Double :: lst
    | 'Z' :: tail -> let c,lst = parse tail in c, Boolean :: lst
    | 'L' :: tail -> let chrs, rest = parse_class tail in
      let c,lst = parse rest in
      c, (Class (String.of_char_list chrs)) :: lst
    | '[' :: tail -> let cls, rest = parse_array tail in
      let c,lst = parse rest in
      c, (Class cls) :: lst
    | ')' :: tail -> 0, []
    | _ -> raise VirtualMachineError
  in
  let c, lst = parse (String.to_list descriptor) in
  c, List.rev lst

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

let component_of_class descriptor =
  let rec aux = function
    | '['::tail -> aux tail
    | 'L'::tail -> String.of_char_list @@ List.slice tail 0 (List.length tail - 1)
    | chrs -> String.of_char_list chrs
  in aux @@ String.to_list descriptor
