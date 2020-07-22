open Batteries
open Enigma

module Machine = Pathway.Make(Base_pair)

let make_machine () =
  let open Machine in
  let right_rotor = Rotor.create ~step:1 KeyPermutation.(cycle [A; G; C; T])
  and middle_rotor = Rotor.create ~step:4 KeyPermutation.((cycle [A; T]) * (cycle [C; G]))
  and left_rotor = Rotor.create ~step:16 KeyPermutation.((cycle [A; C]) * (cycle [T; G]))
  and reflector = Reflector.create KeyPermutation.((cycle [A; G]) * (cycle [T; C]))
  and plugboard = Plugboard.(empty () |> plug T C)
  in
  create ~rotors:[right_rotor; middle_rotor; left_rotor] ~reflector ~plugboard

let encode machine plaintext =
  plaintext
  |> String.enum
  |> List.of_enum
  |> List.map (fun c -> Char.code c |> Base_pair.of_byte)
  |> List.flatten
  |> List.map Machine.(encrypt machine)
  |> List.map Base_pair.to_char
  |> List.iter Printf.(printf "%c")

let decode machine ciphertext =
  let rec list_cut = function
    | a::b::c::d::xs -> [a; b; c; d]::list_cut xs
    | _ -> []
  in

  ciphertext
  |> String.enum
  |> List.of_enum
  |> List.map Base_pair.of_char
  |> List.map Machine.(encrypt machine)
  |> list_cut
  |> List.map Base_pair.to_byte
  |> List.map Char.chr
  |> List.iter Printf.(printf "%c")

let () =
  let text = IO.read_all stdin |> String.strip
  and machine = make_machine ()
  in

  match Sys.argv.(1) with
  | "encode" -> encode machine text
  | "decode" -> decode machine text
  | _ -> failwith "unknown command"
