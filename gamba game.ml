let score a b c n =
  let gta = n > a in
  let gtb = n > b in
  let gtc = n > c in
  let eqa = n = a in
  let eqb = n = b in
  let eqc = n = c in
  if (gta && gtb && not gtc) || (gta && gtc && not gtb) || (gtb && gtc && not gta) then 2
  else if eqa || eqb || eqc then 1
  else 0

let inputInt () = Scanf.scanf "%d " (fun a -> a) (*evil, %d is garbage*)

let round guess =
  let (g1, g2, g3) = ((Random.int 7), (Random.int 7) ,(Random.int 7)) in
  let points = score g1 g2 g3 guess in
  let _ = Printf.printf "You got this many points from this round: %d\n" points in
  points

let rec rounds scoreTotal guessFn =
  let pts = round (guessFn ()) + scoreTotal in
  let _ = Printf.printf "You got this many points in total: %d\n\n" pts in
  rounds pts guessFn

let _ =
  let _ = Random.self_init () in
  rounds 0 read_int (*read_int works unlike scanf :( *)
