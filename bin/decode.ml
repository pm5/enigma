open Batteries
open Enigma

let () =
  let in_file = "bp.txt" in
  let out_file = "out.txt" in
  let inc = File.open_in in_file in
  let outc = File.open_out out_file in
  try
    while true do
      let cs = IO.nread inc 4 in
      List.map Base_pair.of_char [cs.[0]; cs.[1]; cs.[2]; cs.[3]]
      |> Base_pair.to_byte
      |> BatChar.chr
      |> IO.write outc
    done
  with BatInnerIO.No_more_input -> ()
