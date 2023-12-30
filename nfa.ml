open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  List.fold_left (fun acc (start, trans, finish) -> if elem start qs && trans = s then insert finish acc else acc) [] nfa.delta
;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec e_closure_h delta q = match delta with
    | [] -> []
    | (start, trans, finish)::t -> if (q = start && trans = None) then
          union (insert finish (e_closure_h delta finish)) (e_closure_h t q)
        else e_closure_h t q
  in List.fold_left (fun acc curr_state -> union (insert curr_state acc) (e_closure_h nfa.delta curr_state)) [] qs
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let characters = explode s in
  let initial_state = e_closure nfa [nfa.q0] in
  let states = match characters with 
    | [] -> initial_state
    | _ -> List.fold_left (fun a curr_char -> e_closure nfa (move nfa a (Some curr_char))) initial_state characters
  in List.fold_left (fun acc curr_state -> if (elem curr_state nfa.fs) then true else acc) false states
;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc s -> insert (e_closure nfa (move nfa qs (Some s))) acc) [] nfa.sigma
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc s -> insert (qs, Some s, (e_closure nfa (move nfa qs (Some s)))) acc) [] nfa.sigma
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (intersection nfa.fs qs) <> [] then [qs] else []
;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = match work with
  | [] -> dfa
  | h::t -> if h <> [] then (let new_dfa =
                               {
                                 sigma = dfa.sigma;
                                 qs = union (new_states nfa h) dfa.qs;
                                 q0 = dfa.q0;
                                 fs = union (new_finals nfa h) dfa.fs;
                                 delta = union (new_trans nfa h) dfa.delta
                               } in
                             nfa_to_dfa_step nfa new_dfa (union t (new_states nfa h)))
      else nfa_to_dfa_step nfa dfa t
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = 
    {
      sigma = nfa.sigma;
      qs = [e_closure nfa [nfa.q0]];
      q0 = e_closure nfa [nfa.q0];
      fs = [];
      delta = [];
    } in nfa_to_dfa_step nfa dfa [e_closure nfa [nfa.q0]]
;;
