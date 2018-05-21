(* Josh Sloan *)
(* Note: I apologize if the comments make the code difficult to read! *)

(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* base code for hw6 *)

(* ====================================================================== *)
(* UTILITIES                                                              *)
(* ====================================================================== *)
 
let update_fn (x,v) f = 
  fun y -> if x = y then v else f y
;;

(* ====================================================================== *)
(* ABSTRACT SYNTAX                                                        *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)

type loc =  Loc of int ;;

let string_of_loc l = 
  match l  with
   Loc i -> "Loc " ^ (string_of_int i)
;;

(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)

type expression =
    Num of int
  | Deref of loc
  | Plus of expression * expression
  | Not of expression
  | Eq of expression * expression
  | AssignE of loc * expression
;;

let rec string_of_expression e =
  match e with 
    Num (i) -> "Num " ^ (string_of_int i)
  | Deref (al) -> "Deref " ^ (string_of_loc al)
  | Plus (e1,e2) -> "Plus (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e1) ^ ")"
  | Not (e1) -> "Not " ^ (string_of_expression e1)
  | Eq (e1,e2) -> "Eq  (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e1) ^ ")"
  | AssignE (l,e) -> "AssignE (" ^ string_of_loc l ^ ", " ^ string_of_expression e ^ ")"
;;


(* ---------------------------------------------------------------------- *)
(* commands                                                               *)
(* ---------------------------------------------------------------------- *)

type command = 
    Assign of loc * expression
  | Seq of command * command
  | Ite of expression * command * command
  | While of expression * command
  | Skip
;;

let rec string_of_command c = 
  match c with 
    Assign (al,e) -> "Assign (" ^ (string_of_loc al) ^ ", " ^ (string_of_expression e) ^ ")"
  | Seq (c1,c2) -> "Seq (" ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | Ite (e,c1,c2) -> "Ite (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | While (e,c1) -> "While (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ")"
  | Skip  -> "Skip "
;;



(* ====================================================================== *)
(* TYPING                                                                 *)
(* ====================================================================== *)

type expression_types = Boolexp | Intexp ;;

exception TypeError of expression ;;

let well_typed_loc (Loc i) = i > 0 ;;

let rec expression_type e =
  match e with 
      Num (i) -> Intexp
    | Deref (al) -> 
	if (well_typed_loc al) then Intexp else raise (TypeError e)
    | Plus (e1,e2) -> 
	if ((expression_type e1 = Intexp) && (expression_type e2 = Intexp)) then 
	  Intexp
	else
	  raise(TypeError e)
    | Not (e1) ->   
	if (expression_type e1 = Boolexp) then Boolexp else raise(TypeError e)
    | Eq (e1,e2) -> 
	let e1t = (expression_type e1) in
	let e2t = (expression_type e2) in
	  if (e1t = e2t) then Boolexp else raise(TypeError e)
    | AssignE (l,e1) -> 
	(match (well_typed_loc l, expression_type e1) with
	   (true, Intexp) -> Intexp
        | _ ->  raise (TypeError e))
;;

let well_typed_int_exp e = 
  try (expression_type e = Intexp) with 
      TypeError  x -> false
;;

let well_typed_bool_exp e = 
  try (expression_type e = Boolexp) with 
      TypeError x -> false
;;

let rec well_typed_command c = 
  match c with
      Assign (l,e) -> (well_typed_loc l) && (well_typed_int_exp e)
    | Seq(c1,c2) ->  (well_typed_command c1) && (well_typed_command c2)
    | Ite(e,c1,c2) -> (well_typed_bool_exp e) && (well_typed_command c1) && (well_typed_command c2)
    | While(e,c1) -> (well_typed_bool_exp e) && (well_typed_command c1)
    | Skip -> true
;;



(* ====================================================================== *)
(* SEMANTICS                                                              *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* stores                                                                 *)
(* ---------------------------------------------------------------------- *)

(* we model stores as pairs consisting of an int and a function of typ loc -> int.  *) 

type store = Store of  (int * (loc -> int)) ;;

let new_store size init = Store (size, fun i -> init);;

let size (Store (i,f)) =  i;;

let mem (Store (i,f)) = f ;;

let lookup ((Loc i) , s) =
  if (i > size s) || (i <= 0) then
    0
  else
    (mem s) (Loc i)
 ;;

let update (Loc i, v, s) =
  if i > (size s) || (i < 0) then
    s
  else
    Store (size s, update_fn (Loc i, v) (mem s))
;;

(* print_store  prints the locations of store s *)

let print_store (Store (i, f)) = 
  let rec ps j = 
    if j <= i then
      (print_string "Loc";
       print_int j ;
       print_string ": ";
       print_int (f (Loc j)); 
       print_string "\n";
       ps (j + 1))
    else
      ()
  in
 ps 1
;;

(* ---------------------------------------------------------------------- *)
(* meaning of locations                                                   *)
(* ---------------------------------------------------------------------- *)

let meaning_of_loc (Loc i) = (Loc i) ;;

(* ---------------------------------------------------------------------- *)
(* meaning of expresions --                                               *)
(* ---------------------------------------------------------------------- *)

let rec meaning_of_int_expression e s =
 match e with
    Num k -> (k,s)
  | Deref (al) -> (lookup(meaning_of_loc (al),s),s)
  | Plus (e1,e2) -> 
    let (i1,s1) = meaning_of_int_expression e1 s in
    let (i2,s2) = meaning_of_int_expression e2 s in
      (i1+i2,s2)
  | AssignE (l,e1) ->
    let (i,s1) = meaning_of_int_expression e1 s in
    let s2 = update((meaning_of_loc (l)),i,s1) in
      (i,s2)
  | _ -> (1,s) (* should not be needed due to expression_type functionality *)
;;

let rec meaning_of_bool_expression e s =
  match e with
    Not (e1) -> 
      let (b,s') = meaning_of_bool_expression e1 s in
        (not b,s')
  | Eq (e1,e2) ->
    if (well_typed_bool_exp e1 && well_typed_bool_exp e2) ||
       (well_typed_int_exp e1 && well_typed_int_exp e2) then
      if well_typed_bool_exp e1 then (* given first check, e2:Boolexp *)
        let (b1,s1) = meaning_of_bool_expression e1 s in
        let (b2,s2) = meaning_of_bool_expression e2 s1 in
        let br = (b1=b2) in (* compare Boolexps and bind boolean output to br *)
          (br,s2)
      else (* given first check, we know e1,e2: Intexp *)
        let (b1,s1) = meaning_of_int_expression e1 s in
        let (b2,s2) = meaning_of_int_expression e2 s1 in
        let br = (b1=b2) in (* compare Intexps and bind boolean output to br *)
            (br,s2)
   else (false,s) (* only needed for type checking error, expression is not
                     well typed if this returned *)
  | _ -> (true,s) (* should not be needed due to expression_type functionality *)

;;

(* ---------------------------------------------------------------------- *)
(* meaning of commands                                                    *)
(* ---------------------------------------------------------------------- *)

let rec meaning_of_command c s =  
  match c with 
      Assign (al,e) -> 
	let (i,s') =  meaning_of_int_expression e s in
	let loc = meaning_of_loc al in
	  update (loc,i,s')
    | Seq (c1,c2) -> meaning_of_command c2 (meaning_of_command c1 s)
    | Ite (e,c1,c2) -> 
	let (b,s') = meaning_of_bool_expression e s in
          if b then
	    meaning_of_command c1 s'
	  else
	    meaning_of_command c2 s'
    | While (e,c1) -> 
	let (b,s') = meaning_of_bool_expression e s in
	  if b then
	    meaning_of_command (Seq(c1,While(e,c1))) s'
	  else
	    s'
    | Skip  -> s
;;
