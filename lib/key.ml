open Batteries

module Byte = struct
  type t = int
  let all () = 0--255
  let all_int = all
  let to_int x = x
  let of_int x = x
end

module Letter = struct
  type t = char
  let all () = Char.('A'--'Z')
  let all_int () = Enum.map Char.code (all ())
  let to_int = Char.code
  let of_int = Char.chr
end

module Simple = struct
  type t = char
  let all () = ['A'; 'S'; 'D'; 'F'] |> List.enum
  let all_int () = all () |> Enum.map Char.code
  let to_int = Char.code
  let of_int = Char.chr
end

module Base_pair = struct
  type t = char
  let all () = ['A'; 'T'; 'C'; 'G'] |> List.enum
  let all_int () = all () |> Enum.map Char.code
  let to_int = Char.code
  let of_int = Char.chr
end
