type charClass =  Ranger | Warrior | Magician
type charObject = GoldCoins | ChickenWings | Sponges
type charBag = (charObject * int) list
type race = Golem | Boar | MosquitoesSwarm of int

exception Death of string

type character = {
  cls: charClass;
  bag: charBag;
  hp: int;
  xp: int
}

type monster = {
  rc: race;
  hp: int;
  obj: charObject
}


let rec addObjectToBag (b: charBag) (o: charObject) = match b with
  | [] -> [(o,1)]
  | (ob, n)::t when ob = o -> (o, n + 1)::t
  | e :: t -> e :: addObjectToBag t o


let objToString = function
  | GoldCoins -> "coins"
  | ChickenWings -> "chicken wing"
  | Sponges -> "Sponges"

let raceToString = function
  | Golem -> "Golem"
  | MosquitoesSwarm n -> Printf.sprintf "swarm of %d Mosquitoes" n
  | Boar -> "Boar"

let print_bag (b: charBag) = List.iter (fun (o:  charObject * int) ->
  let obj, count = o in
  let objType = objToString obj in
  Printf.printf "%d %s\n" count objType
) b

let random_monster () =
  let x = (Random.int 3) in
  match x with
  | 0 -> Golem
  | 1 -> Boar
  | 2 -> MosquitoesSwarm (Random.int 10)
  | _ -> Golem (* shouldnt be able to happen*)

let hit (c:character) =
  let damage, chance = match c.cls with
    | Warrior -> (10, 0.3)
    | Ranger -> (4, 0.7)
    | Magician -> (5,0.5) in
  let roll = Random.float 1.0 in
    if roll < (chance +. 0.05 *. (float_of_int c.xp)) then damage else 0

let monster_hit (m: monster) =
  let r = m.rc in
  match r with
    | Golem -> 4
    | MosquitoesSwarm n ->  n / 2 (*no idea how to make it swarm*)
    | Boar -> 2

let fight (c: character) (m: monster) =
  let battleOne = m.hp - (hit c) in
  if battleOne <= 0 then {c with xp = c.xp + 5; bag = addObjectToBag c.bag m.obj}
  else let battleTwo = c.hp - (monster_hit m) in
  if battleTwo <= 0 then raise (Death "The monster killed you") else {c with hp = battleTwo; xp = c.xp + 5}

let unfortunate_encounter (c: character) =
  let rc = random_monster () in
  let hp = 3 in
  let o = match rc with
  | Golem -> GoldCoins
  | MosquitoesSwarm n -> ChickenWings
  | Boar -> Sponges in
  let m: monster = {
    rc;
    hp;
    obj= o
  } in
  let _ = Printf.printf "A %s appeared with %d health and holds %s \n" (raceToString rc) hp (objToString o) in
  fight c m


let rec playRounds n c = match n with
  | 0 -> c
  | n -> playRounds (n - 1) (unfortunate_encounter c)

let _ =
  let _ = Random.self_init () in
  let rounds = 4 in
  let c = { (* could make this customizable through user input ig *)
    cls= Ranger;
    bag= [];
    hp= 10;
    xp= 0
  } in
  try let c = playRounds rounds c in
    let _ = Printf.printf "\nLook at all that LOOT!\n" in
    (print_bag c.bag)
  with Death s -> let _ = Printf.printf "%s \n" s in ()
