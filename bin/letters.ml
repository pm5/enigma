open Batteries
open Enigma

module Machine = Pathway.Make(Key.Letter)

let () =
  let plaintext = IO.read_line stdin |> String.strip
  in

  let open Machine in
  let right_rotor = Rotor.create ~step:1 KeyPermutation.
    ( cycle ['A'; 'E'; 'L'; 'T'; 'P'; 'H'; 'Q'; 'X'; 'R'; 'U']
    * cycle ['B'; 'K'; 'N'; 'W']
    * cycle ['C'; 'M'; 'O'; 'Y']
    * cycle ['D'; 'F'; 'G']
    * cycle ['I'; 'V']
    * cycle ['J'; 'Z']
    )
  and middle_rotor = Rotor.create ~step:4 KeyPermutation.
    ( cycle ['B'; 'J']
    * cycle ['C'; 'D'; 'K'; 'L'; 'H'; 'U'; 'P']
    * cycle ['E'; 'S'; 'Z']
    * cycle ['F'; 'I'; 'X'; 'V'; 'Y'; 'O'; 'M'; 'W']
    * cycle ['G'; 'R']
    * cycle ['N'; 'T']
    )
  and left_rotor = Rotor.create ~step:16 KeyPermutation.
    ( cycle ['A'; 'B'; 'D'; 'H'; 'P'; 'E'; 'J'; 'T']
    * cycle ['C'; 'F'; 'L'; 'V'; 'M'; 'Z'; 'O'; 'Y'; 'Q'; 'I'; 'R'; 'W'; 'U'; 'K'; 'X'; 'S'; 'G']
    )
  and reflector = Reflector.create KeyPermutation.
    ( cycle ['A'; 'Y']
    * cycle ['B'; 'R']
    * cycle ['C'; 'U']
    * cycle ['D'; 'H']
    * cycle ['E'; 'Q']
    * cycle ['F'; 'S']
    * cycle ['G'; 'L']
    * cycle ['I'; 'P']
    * cycle ['J'; 'X']
    * cycle ['K'; 'N']
    * cycle ['M'; 'O']
    * cycle ['T'; 'Z']
    * cycle ['V'; 'W']
    )
  and plugboard = Plugboard.(empty () |> plug 'S' 'D')
  in
  let machine = create ~rotors:[right_rotor; middle_rotor; left_rotor] ~reflector ~plugboard
  in

  plaintext |> String.map Machine.(encrypt machine) |> String.iter Printf.(printf "%c")

