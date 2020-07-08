open Batteries
open Enigma

let () =
  let in_file = Sys.argv.(1) in
  let inc = File.open_in in_file in
  try
    while true do
      let cs = IO.nread inc 4 in
      List.map Base_pair.of_char [cs.[0]; cs.[1]; cs.[2]; cs.[3]]
      |> Base_pair.to_byte
      |> BatChar.chr
      |> Printf.printf "%c"
    done
  with BatInnerIO.No_more_input -> ()
