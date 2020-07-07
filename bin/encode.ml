open Batteries
open Enigma

module Machine = Pathway.Make(Pathway.Key)

let () =
  let in_file = "words.csv" in
  let rotor = Machine.right_rotor () in
  let inc = File.open_in in_file in
  try
    while true do
      IO.read_byte inc
          |> Machine.Rotor.pass rotor
          |> Base_pair.of_byte
          |> List.map Base_pair.to_char
          |> List.iter (fun c -> Printf.printf "%c" c)
    done
  with BatInnerIO.No_more_input -> ()
