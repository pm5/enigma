open Batteries
open Enigma

module Machine = Pathway.Make(Key.Base_pair)

let () =
  let plaintext = IO.read_line stdin |> String.strip
  in

  let open Machine in
  let right_rotor = Rotor.create ~step:1 KeyPermutation.(cycle ['A'; 'G'; 'C'; 'T'])
  and middle_rotor = Rotor.create ~step:4 KeyPermutation.((cycle ['A'; 'T']) * (cycle ['C'; 'G']))
  and left_rotor = Rotor.create ~step:16 KeyPermutation.((cycle ['A'; 'C']) * (cycle ['T'; 'G']))
  and reflector = Reflector.create KeyPermutation.((cycle ['A'; 'G']) * (cycle ['T'; 'C']))
  and plugboard = Plugboard.(empty () |> plug 'T' 'C')
  in
  let machine = create ~rotors:[right_rotor; middle_rotor; left_rotor] ~reflector ~plugboard
  in

  plaintext |> String.map Machine.(encrypt machine) |> String.iter Printf.(printf "%c")

