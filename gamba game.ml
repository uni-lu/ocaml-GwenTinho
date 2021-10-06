let score a b c n =
  let gta = n > a in
  let gtb = n > b in
  let gtc = n > c in
  let eqa = n = a in
  let eqb = n = b in
  let eqc = n = c in
  if gta && gtb || gta && gtc || gtb && gtc then 2 else if eqa || eqb || eqc then 1 else 0


let _ =
  let (g1, g2, g3) = ((Random.int 7), (Random.int 7) ,(Random.int 7)) in (* are those actual different random numbers*)
  let x = Scanf.scanf "%d" (fun a -> a) in
  let _ = Printf.printf "%d\n" (score g1 g2 g3 x) in
  ()
