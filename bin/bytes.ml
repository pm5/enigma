open Batteries
open Enigma

module Machine = Pathway.Make(Key.Byte)

let make_machine () =
  let open Machine in
  let right_rotor = Rotor.create ~step:1 KeyPermutation.(cycle [0; 2])
  and middle_rotor = Rotor.create ~step:4 KeyPermutation.((cycle [1; 3]) * (cycle [112; 113]))
  and left_rotor = Rotor.create ~step:16 KeyPermutation.((cycle [55; 66]) * (cycle [47; 201]))
  and reflector = Reflector.create KeyPermutation.((cycle [71; 3]) * (cycle [93; 101]))
  and plugboard = Plugboard.(empty () |> plug 225 101)
  in
  create ~rotors:[right_rotor; middle_rotor; left_rotor] ~reflector ~plugboard

let () =
  let plaintext = IO.read_all stdin |> String.strip
  and machine = make_machine ()
  in

  plaintext
  |> String.enum
  |> List.of_enum
  |> List.map Char.code
  |> List.map Machine.(encrypt machine)
  |> List.map Char.chr
  |> List.iter Printf.(printf "%c")
