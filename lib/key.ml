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
