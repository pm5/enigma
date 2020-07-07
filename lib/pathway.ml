open Batteries

module Make(P : Permutation.Permutable with type t := int) = struct

  module KeyPermutation = Permutation.Make(P)

  module Rotor = struct
    type t =
      { mutable phi : KeyPermutation.t
      ; step : int
      ; mutable count : int
      ; rotate : KeyPermutation.t
      }

    let create ~step phi =
      { phi
      ; step
      ; count = 0
      ; rotate = KeyPermutation.rotate ()
      }

    let step rotor =
      let open KeyPermutation in
      rotor.phi <- rotor.phi * rotor.rotate;
      rotor.count <- 0

    let pass rotor key =
      let key' = KeyPermutation.permute rotor.phi key in
      rotor.count <- rotor.count + 1;
      if rotor.count >= rotor.step
      then step rotor;
      key'
  end

  module Reflector = struct
    type t = KeyPermutation.t

    let create phi = phi

    let pass refl key = KeyPermutation.permute refl key
  end

  let right_rotor () = Rotor.create ~step:1 @@
    KeyPermutation.cycle [
      121; 33; 57; 90; 210; 13; 97
    ]

  let middle_rotor () = Rotor.create ~step:10 @@
    KeyPermutation.identity ()

  let left_rotor () = Rotor.create ~step:100 @@
    KeyPermutation.identity ()

  let reflector = Reflector.create (KeyPermutation.identity ())
end
