let rec sort lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt lst = (*just traverses along the list and inserts once it fits*)
   match lst with
     [] -> [elt]
   | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail;;

(*insertion sort from SO, kinda neat*)

List.iter (Printf.printf "%d \n") (sort [ 1 ; 3 ; 9 ; 2 ; 5 ; 4; 4; 8 ; 4 ])
