open Batteries
open Ext

module type Permutable = sig
  type t
  val all : unit -> t Enum.t
  val all_int : unit -> int Enum.t
  val to_int : t -> int
  val of_int : int -> t
end

module Make(P : Permutable) = struct
  module IntMap = Map.Make(Int)

  type t = BatInt.t IntMap.t

  let identity () =
    Enum.fold (fun acc x ->
      IntMap.add x x acc
    ) IntMap.empty P.(all_int ())

  let cycle xs =
    let xs' = List.map P.to_int xs in
    let open List in
    let m = fold_left2 (fun acc src dst ->
      IntMap.add src dst acc
        ) (identity ()) (butlast xs') (tl xs')
    in
    IntMap.add (last xs') (hd xs') m

  let transp x y =
    let x' = P.to_int x
    and y' = P.to_int y
    in
    identity ()
    |> IntMap.add x' y'
    |> IntMap.add y' x'

  let inverse phi =
    IntMap.fold (fun src dst ->
      IntMap.add dst src
    ) phi (identity ())

  let permute_int phi x =
    try
      IntMap.find x phi
    with Not_found -> failwith "invalid element"

  let permute phi x = permute_int phi P.(to_int x) |> P.of_int

  let mul pa pb =
    Enum.fold (fun acc src ->
      let dst = permute_int pb src |> permute_int pa in
      IntMap.add src dst acc
    ) (identity ()) P.(all_int ())

  let ( * ) = mul

  let conj x phi = (inverse x) * phi * x

  let rotate () : t =
    P.all () |> List.of_enum |> cycle
end
