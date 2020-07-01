open Batteries

module Key = struct
  type t = int
  let all () = 0--255
end


module KeyPermutation = Permutation.Make(Key)

module type Rotor = sig
end

module Reflector = struct
end
