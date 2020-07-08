open Batteries

module Make(P : Permutation.Permutable) = struct

  module KeyPermutation = Permutation.Make(P)

  module Rotor = struct
    type t =
      { mutable forward : KeyPermutation.t
      ; step : int
      ; mutable count : int
      ; rotate : KeyPermutation.t
      }

    let create ~step perm =
      { forward = perm
      ; step
      ; count = 0
      ; rotate = KeyPermutation.rotate ()
      }

    let step rotor =
      rotor.count <- rotor.count + 1;
      if rotor.count >= rotor.step
      then begin
        let open KeyPermutation in
        rotor.forward <- rotor.forward * rotor.rotate;
        rotor.count <- 0
      end

    let forward rotor key =
      KeyPermutation.permute rotor.forward key

  end

  module Reflector = struct
    type t = KeyPermutation.t

    let create phi = phi

    let pass refl key = KeyPermutation.permute refl key
  end

  module Plugboard = struct
    type t = KeyPermutation.t

    let empty () = KeyPermutation.identity ()

    let plug pb a b =
      let open KeyPermutation in
      pb * (transp a b)
  end

  let create ~_rotors ~_reflector ~_plugboard = failwith "not implemented"

  let right_rotor () = Rotor.create ~step:1 @@
    KeyPermutation.identity ()

  let middle_rotor () = Rotor.create ~step:10 @@
    KeyPermutation.identity ()

  let left_rotor () = Rotor.create ~step:100 @@
    KeyPermutation.identity ()

  let reflector = Reflector.create (KeyPermutation.identity ())
end
