module Key = Char

module Permutation = struct
  type 'a t = 'a * 'a array

  let permute x =
end

module type Rotor = sig
  type t
  val make : Key.t Permutation.t -> t
  val feed : Key.t -> Key.t
  val rotate : t -> t
end

module Reflector = struct
end
