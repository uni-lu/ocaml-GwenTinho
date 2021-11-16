let rec extGcd a b = match a with
    | 0 -> (b,0,1)
    | a ->
      let d,x,y = extGcd (b mod a) a in
      (d, y - (b/a) * x, x)

let _ =
  let v,x,y = extGcd 35 15 in
  let _ = Printf.printf "%d %d %d\n"  v x y in ()
