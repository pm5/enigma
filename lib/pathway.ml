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

    let backward rotor key =
      let inv = KeyPermutation.inverse rotor.forward in
      KeyPermutation.permute inv key

  end

  module Reflector = struct
    type t = KeyPermutation.t

    let empty () = KeyPermutation.identity ()

    let create phi = phi

    let forward = KeyPermutation.permute
    let backward = forward
  end

  module Plugboard = struct
    type t = KeyPermutation.t

    let empty () = KeyPermutation.identity ()

    let plug a b pb =
      let open KeyPermutation in
      pb * (transp a b)

    let forward = KeyPermutation.permute
    let backward = forward
  end

  type t =
    { rotors : Rotor.t list
    ; reflector : Reflector.t
    ; plugboard : Plugboard.t
    }

  let create ~rotors ~reflector ~plugboard =
    { rotors
    ; reflector
    ; plugboard
    }

  let encrypt m key =
    List.iter Rotor.step m.rotors;
    key
    |> Plugboard.forward m.plugboard
    |> fun key ->
        List.fold_left (fun k rotor -> Rotor.forward rotor k) key m.rotors
    |> Reflector.forward m.reflector
    |> fun key ->
        List.fold_right (fun rotor k -> Rotor.backward rotor k) m.rotors key
    |> Plugboard.backward m.plugboard

end
