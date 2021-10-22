let score a b c n =
  let gta = n > a in
  let gtb = n > b in
  let gtc = n > c in
  let eqa = n = a in
  let eqb = n = b in
  let eqc = n = c in
  if gta && gtb || gta && gtc || gtb && gtc then 2 else if eqa || eqb || eqc then 1 else 0


let printInt = Printf.printf "%d\n"
let printString = Printf.printf "%s\n"
let inputInt () = Scanf.scanf "%d " (fun a -> a)

let round ()=
  let (g1, g2, g3) = ((Random.int 7), (Random.int 7) ,(Random.int 7)) in
  let x = read_int () in
  let points = score g1 g2 g3 x in
  let _ = printInt points in
  points

let rec rounds scoreTotal =
  let pts = round () in
  let _ = printString "You got this many points now" in
  let _ = printInt (scoreTotal + pts) in
  rounds (scoreTotal + pts)

let _ =
  let _ = Random.self_init () in
  rounds 0
