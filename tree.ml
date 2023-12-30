type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with
  | Leaf -> init
  | Node (l, x, r) -> f (tree_fold f init l) x (tree_fold f init r)

let map tree f = tree_fold (fun left x right -> Node(left, f x, right)) Leaf tree
let mirror tree = tree_fold (fun left x right -> Node(right, x, left)) Leaf tree
let in_order tree = tree_fold (fun left x right -> left @ [x] @ right) [] tree
let pre_order tree  = tree_fold (fun left x right -> x :: left @ right) [] tree
let compose tree = tree_fold (fun left value right -> fun x -> right (value (left x)) ) (fun x -> x) tree

let depth tree  = tree_fold (fun left x right -> 1 + (max left right)) 0 tree

(* Assume complete tree *)
let trim tree n = snd (tree_fold (fun (d1, left) value (d1, right) -> 
    if d1 <= n then (d1-1, Node (left, value, right))
    else (d1-1, Leaf)) (depth tree, Leaf) tree)

(* Helper function for from_pre_in *)
let rec split lst v = match lst with
  | [] -> ([], [])
  | h::t ->
      if h = v then ([], t)
      else let (left, right) = split t v in (h::left, right)

(*Helper function for from_pre_in. Splits a list without removing v. left is all elements before v, right is all elements after and including v *)  
let rec separate lst v = match lst with
  | [] -> ([], [])
  | h::t ->
      if h = v then ([], lst)
      else let (left, right) = separate t v in (h::left, right)

(*Helper function for from_pre_in. Returns index of an element in list*)
let rec find x lst = match lst with
  | [] -> raise (Failure "Not Found")
  | h::t -> if x = h then 0 else 1 + find x t

let rec from_pre_in pre in_ord = match pre with
  | [] -> Leaf
  | h::t -> (*first value of pre is root*)
      let (left_in, right_in) = split in_ord h in (*split in_ord into left and right subtree at root*)
      if right_in = [] then (*case for in_ord having no right subtree*)
        let (left_pre, right_pre) = (t, []) in 
        let left_tree = from_pre_in left_pre left_in in (*recurse with pre as left_pre and in_ord as left_in*)
        let right_tree = from_pre_in right_pre right_in in (*recurse with pre as right_pre and in_ord as right_in*)
        Node (left_tree, h, right_tree) 
      else                                            
        let (left_pre, right_pre) = separate t (List.nth t (find h in_ord)) in (*split rest of pre into left and right subtree*)
        let left_tree = from_pre_in left_pre left_in in (*recurse with pre as left_pre and in_ord as left_in*)
        let right_tree = from_pre_in right_pre right_in in (*recurse with pre as right_pre and in_ord as right_in*)
        Node (left_tree, h, right_tree);;
