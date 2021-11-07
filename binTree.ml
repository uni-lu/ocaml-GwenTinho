type 'a bin = Leaf | Node of 'a * 'a bin * 'a bin


(*inorder call for each node*)
let rec iterTree f bt = match bt with
  | Leaf -> ()
  | Node (v,l,r) ->
    let _ = iterTree f l in
    let _ = f v in
    iterTree f r

let rec printTree = iterTree (Printf.printf "%d \n")
(*let _ = printTree (Node (1,Node (2,Node (10,Leaf,Leaf),Node (5,Leaf,Leaf)),Node (3, Leaf, Leaf)))*)

let foldTree init f tree =
  let rec aux acc tree = match tree with
    | Leaf -> acc
    | Node (v,l,r) -> f v (f (aux acc l) (aux acc r)) in
  aux init tree

let rec mapTree f tree = match tree with
    | Leaf -> tree
    | Node (v,l,r) -> Node (f v, mapTree f l, mapTree f r)

(* figure out how to convert a list to a binary tree???*)

let add x y = x + y
let tree = (Node (1,Node (2,Node (10,Leaf,Leaf),Node (15,Leaf,Leaf)),Node (3, Leaf, Leaf)))

let _ = printTree tree
let _ = printTree (mapTree (fun x -> x * 10) tree)
