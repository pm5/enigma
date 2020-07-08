open Printf
open Enigma

module Machine = Pathway.Make(Key.Letter)

let () =
  let open Machine in
  let perm = KeyPermutation.(cycle ['A'; 'T'; 'C'; 'G'] |> conj (transp 'T' 'G')) in
  let rotor = Rotor.create ~step:1 perm in
  printf "%c" Rotor.(forward rotor 'T'); Rotor.step rotor;
  printf "%c" Rotor.(forward rotor 'T'); Rotor.step rotor;
  printf "%c" Rotor.(forward rotor 'T'); Rotor.step rotor;
  printf "%c" Rotor.(forward rotor 'T'); Rotor.step rotor;
  printf "%c" Rotor.(forward rotor 'T'); Rotor.step rotor;
  printf "%c" Rotor.(forward rotor 'T');
  printf "%c" Rotor.(forward rotor 'T');
  printf "%c" Rotor.(forward rotor 'T')
