type 'a bin = Leaf | Node of 'a * 'a bin * 'a bin

let rec printTree bt = match bt with
  | Leaf -> ()
  | Node (v,l,r) ->
    let _ = printTree l in
    let _ = Printf.printf "%d \n" v in
    printTree r

let _ = printTree (Node (1,Node (2,Node (10,Leaf,Leaf),Node (5,Leaf,Leaf)),Node (3, Leaf, Leaf)))
