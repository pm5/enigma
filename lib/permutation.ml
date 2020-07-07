open Batteries
open Ext

module type Permutable = sig
  type t
  val all : unit -> t Enum.t
end

module Make(P : Permutable with type t := int) = struct
  module IntMap = Map.Make(Int)

  type t = BatInt.t IntMap.t

  let identity () =
    Enum.fold (fun acc x ->
      IntMap.add x x acc
    ) IntMap.empty P.(all ())

  let cycle xs =
    let open List in
    let m = fold_left2 (fun acc src dst ->
      IntMap.add src dst acc
        ) (identity ()) (butlast xs) (tl xs)
    in
    IntMap.add (last xs) (hd xs) m

  let transp x y =
    identity ()
    |> IntMap.add x y
    |> IntMap.add y x

  let inverse phi =
    IntMap.fold (fun src dst ->
      IntMap.add dst src
    ) phi (identity ())

  let permute phi x =
    try
      IntMap.find x phi
    with Not_found -> failwith "invalid element"

  let mul pa pb =
    Enum.fold (fun acc src ->
      let dst = permute pb src |> permute pa in
      IntMap.add src dst acc
    ) (identity ()) P.(all ())

  let ( * ) = mul

  let conj x phi = (inverse x) * phi * x

  let rotate () : t =
    P.all () |> List.of_enum |> cycle
end
