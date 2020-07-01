(** Extensions to Stdlib and Batteries. *)

module List = struct
  include BatList

  (** [butlast xs] is [xs] without its last element, or Failure if [xs] is empty. *)
  let butlast lst =
    let rec aux acc = function
      | [] -> failwith "empty list"
      | _::[] -> List.rev acc
      | a::xs' -> aux (a::acc) xs'
    in
    aux [] lst
end
