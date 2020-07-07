open Printf
open Enigma

module Machine = Pathway.Make(Key.Bytes)

let () =
  let open Machine in
  let perm = KeyPermutation.(cycle [15; 20; 25; 30] |> conj (transp 20 30)) in
  let rotor = Rotor.create ~step:1 perm in
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
  printf "%d\n" Rotor.(pass rotor 25);
