open Batteries
open Enigma

module Machine = Pathway.Make(Key.Simple)

let () =
  let open Machine in
  let right_rotor = Rotor.create ~step:1 KeyPermutation.(cycle ['A'; 'F'; 'D'; 'S'])
  and middle_rotor = Rotor.create ~step:4 KeyPermutation.((cycle ['A'; 'S']) * (cycle ['D'; 'F']))
  and left_rotor = Rotor.create ~step:16 KeyPermutation.((cycle ['A'; 'D']) * (cycle ['S'; 'F']))
  and reflector = Reflector.create KeyPermutation.((cycle ['A'; 'F']) * (cycle ['S'; 'D']))
  and plugboard = Plugboard.(empty () |> plug 'S' 'D')
  in
  let machine = create ~rotors:[right_rotor; middle_rotor; left_rotor]
  ~reflector ~plugboard
  in

  IO.read_line stdin
  |> String.strip
  |> String.map Machine.(encrypt machine)
  |> String.iter Printf.(printf "%c")
