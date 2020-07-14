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

let () =
  let plaintext = IO.read_all stdin |> String.strip
  and machine = make_machine ()
  in

  plaintext
  |> String.enum
  |> List.of_enum
  |> List.map (fun c -> Char.code c |> Base_pair.of_byte)
  |> List.flatten
  |> List.map Machine.(encrypt machine)
  |> List.map Base_pair.to_char
  |> List.iter Printf.(printf "%c")
