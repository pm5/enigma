type t = A | T | C | G

let to_char = function
  | A -> 'A'
  | T -> 'T'
  | C -> 'C'
  | G -> 'G'

let of_char = function
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> failwith "invalid"

let of_byte b =
  let rec aux acc x =
    if x = 0 then acc
    else begin
      let acc' = (
        match x mod 4 with
        | 0 -> A
        | 1 -> T
        | 2 -> C
        | 3 -> G
        | _ -> failwith "invalid"
      )::acc in
      let x' = x / 4 in
      aux acc' x'
    end
  in
  aux [] b

let to_byte bp =
  List.fold_left (fun acc x ->
    match x with
    | A -> acc * 4
    | T -> acc * 4 + 1
    | C -> acc * 4 + 2
    | G -> acc * 4 + 3
  ) 0 bp

