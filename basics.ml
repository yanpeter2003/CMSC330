open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with 
    (a, b, c) -> (c, b, a);;

let is_even x = 
  if x mod 2 = 0 then true
  else false;;

let area p q  = match p, q with
    (pa , pb), (qa,qb) -> ((abs (pa-qa)) * (abs (pb-qb)));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

(*All correct*)
let rec fibonacci n = 
  if n < 2 then n
  else fibonacci (n - 1) + fibonacci (n - 2);;

let rec pow x p = 
  if p = 0 then 1 
  else x * pow x (p - 1);;

let is_prime x =
  let rec noDivisors y =
    y * y > x || (x mod y != 0 && noDivisors (y + 1))
  in
  x >= 2 && noDivisors 2;;
  
let rec maxFuncChain init funcs = match funcs with
  | [] -> init
  | h::t ->  if h init > init then maxFuncChain (h init) t
      else maxFuncChain init t;;  
(*****************)
(* Part 3: Lists *)
(*****************)

let rec reverse lst = match lst with
  | [] -> []
  | h::t -> reverse t @ [h];;

let rec merge lst1 lst2 = match lst1, lst2 with
  | [], l -> l
  | l, [] -> l
  | h1 :: t1, h2 :: t2 ->
      if h1 < h2 then 
        h1 :: merge t1 (h2 :: t2) else 
        h2 :: merge (h1 :: t1) t2;;

let jumping_tuples lst1 lst2 = 
  let rec aux index lst1 lst2 = match lst1, lst2 with
    | [], l -> []
    | l, [] -> []
    | (f1, s1) :: t1, (f2, s2) :: t2 -> 
        if is_even index then s2 :: aux (index + 1) t1 t2
        else f1 :: aux (index + 1) t1 t2 in aux 0 lst1 lst2 ;;

let rec is_palindrome lst = 
  lst = reverse lst;;

let rec square_primes lst = match lst with
  | [] -> []
  | h::t-> if is_prime h then 
        (h, h*h) :: square_primes t
      else square_primes t;;

let rec flatten lst = match lst with
  | [] -> []
  | h::t -> h @ flatten t;;

let partition p lst =
  let rec part l1 l2 = function
    | [] -> (reverse l1, reverse l2)
    | h :: lst -> if p h then part (h :: l1) l2 lst 
        else part l1 (h :: l2) lst in part [] [] lst;;

(*****************)
(* Part 4: HOF *)
(*****************)

(*All correct*)
let is_present lst x = 
  map (fun ele -> if ele = x then 1 else 0) lst;;

(*All correct*)
let count_occ lst target = 
  fold (fun acc ele -> if ele = target then acc + 1 else acc) 0 lst;;

(*Helper function for uniq to find if a value is in a list*)
let rec find x lst = match lst with 
  | [] -> false
  | h::t -> if h = x then true
      else find x t;;

(*All correct*)
let uniq lst = 
  fold_right (fun ele lst2 -> if find ele lst2 then lst2 else ele :: lst2) lst [];;
