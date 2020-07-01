open Enigma

let () =
  let open Pathway in
  let perm = KeyPermutation.(cycle [15; 20; 25; 30] |> conj (transp 20 30)) in
  print_int KeyPermutation.(permute perm 25)
