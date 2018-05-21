(* Josh Sloan *)

(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* base code for HW 8 *)

(* ====================================================================== *)
(* UTILITIES                                                              *)
(* ====================================================================== *)

let update (x,v) f = fun y -> if x = y then v else f y ;;

let rec map f l = 
  match l with 
      [] -> []
    | h::t -> (f h)::(map f t)
;;

let rec member x l =
  match l with
      [] -> false
    | h::t -> if h = x then true else member x t
;;

let rec disjoint l m = 
  match l with
      [] -> true
    | h::t -> not (member h m) && (disjoint t m)
;;

let disjoint_union l m = 
 if disjoint l m then 
   (l @ m)
 else
   raise (Failure "disjoint_union: not disjoint.")
;;


(* ====================================================================== *)
(* ABSTRACT SYNTAX                                                        *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)

type loc =  Loc of int ;;

let well_typed_loc (Loc i) = i > 0 ;;

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
  | Id of string
;;

let rec string_of_expression e =
  match e with 
    Num (i) -> "Num " ^ (string_of_int i)
  | Deref (al) -> "Deref " ^ (string_of_loc al)
  | Plus (e1,e2) -> "Plus (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e2) ^ ")"
  | Not (e1) -> "Not " ^ (string_of_expression e1)
  | Eq (e1,e2) -> "Eq  (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e2) ^ ")"
  | Id s -> "Id " ^ s
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

(* ---------------------------------------------------------------------- *)
(* declarations                                                           *)
(* ---------------------------------------------------------------------- *)

type declaration = 
    Fun of string * expression 
  | Comma of declaration * declaration 
  | Semi of declaration * declaration
;;

let rec string_of_declaration d = 
  match d with
      Fun(i,e) -> "Fun " ^ i ^ " = " ^ (string_of_expression e)
    | Comma(d1,d2) -> (string_of_declaration d1) ^ ", " ^ string_of_declaration d2
    | Semi(d1,d2) -> (string_of_declaration d1) ^ "; " ^ string_of_declaration d2
;;

(* ---------------------------------------------------------------------- *)
(* programs                                                               *)
(* ---------------------------------------------------------------------- *)

type program = Prog of declaration * command;;

let string_of_program (Prog(d,c)) = 
  (string_of_declaration d) ^ " in " ^ (string_of_command c)
;;

(* ====================================================================== *)
(* TYPE CHECKING                                                          *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* type assignments                                                       *)
(* ---------------------------------------------------------------------- *)

type expression_types = Boolexp | Intexp ;;

let string_of_expression_types t = 
  match t with
      Boolexp -> "Boolexp"
    | Intexp -> "Intexp"
;;

exception TypeError of expression ;;

type type_assignment = TA of ((string * expression_types) list) ;;

let update_ta (i,v) (TA f) = TA ((i,v)::f);;

let rec type_of_id (TA f) id = 
  match f with
      [] -> (raise (Failure ("no such type assignment: " ^ id))  : expression_types)
    | (id',ty)::t -> if id = id' then ty else (type_of_id (TA t) id)
;;

let ta_names (TA f) = map fst f;;

let union_ta (TA p1) (TA p2) = 
  if (disjoint (ta_names (TA p1)) (ta_names (TA p2))) then 
    (TA (p1 @ p2)) 
  else 
    raise (Failure "union_ta: undefined -- not disjoint.")
;;


(* pi0 is the empty type assignment *)
let pi0  = TA [] ;;

let print_ta (TA f) =
  let rec pta f =
    match f with
	[] -> ()
      | h::t -> 
	  (print_string (fst h);
	   print_string ":"; 
           print_string (string_of_expression_types (snd h));
           (if not (t = []) then 
	      print_string ", "
	    else
	      ());
	   pta  t
	  )

  in
    print_string "{";
    pta f;
    print_string "}"
;;



(* ---------------------------------------------------------------------- *)
(* typing expressions                                                     *)
(* ---------------------------------------------------------------------- *)

let rec expression_type e pi =
  match e with 
    Num (i) -> Intexp
  | Deref (al) -> 
      if (well_typed_loc al) then Intexp else raise (TypeError e)
  | Plus (e1,e2) -> 
      if ((expression_type e1 pi = Intexp) && (expression_type e2 pi = Intexp)) then 
	Intexp
      else
	raise(TypeError e)
  | Not (e1) ->   
      if (expression_type e1 pi = Boolexp) then Boolexp else raise(TypeError e)
  | Eq (e1,e2) -> 
      let e1t = (expression_type e1 pi) in
      let e2t = (expression_type e2 pi) in
      if (e1t = e2t) then Boolexp else raise(TypeError e)
  | Id name -> type_of_id pi name
;;

let well_typed_expression e pi = 
  try (let _ = expression_type e pi in true ) with TypeError e -> false 
;;

(* ---------------------------------------------------------------------- *)
(* typing declarations                                                    *)
(* ---------------------------------------------------------------------- *)

(* write declaration type *)

let rec declaration_type d pi =
   match d with
      Fun(i,e) -> TA [(i,(expression_type e pi))]

    | Comma(d1,d2) -> 
      union_ta (declaration_type d1 pi) (declaration_type d2 pi)

    | Semi(d1,d2) ->
      let pi1 = (declaration_type d1 pi) in
      let pi2 = (union_ta pi pi1) in
      union_ta pi1 (declaration_type d2 pi2)
;;


let well_typed_declaration d pi =
  try (let _ = declaration_type d pi in true ) with TypeError e -> false | _ -> false  
;;
  
(* ---------------------------------------------------------------------- *)
(* typing commands                                                        *)
(* ---------------------------------------------------------------------- *)

let rec well_typed_command c pi =
  match c with 
    Assign (al,e) -> 
      (well_typed_loc al) 
	&& (well_typed_expression e pi) 
	&& (expression_type e pi = Intexp)
  | Seq (c1,c2) -> (well_typed_command c1 pi) && (well_typed_command c2 pi)
  | Ite (e,c1,c2) -> 
      (well_typed_expression e pi) 
	&& (expression_type e pi = Boolexp) 
	&& (well_typed_command c1 pi) 
	&& (well_typed_command c2 pi)
  | While (e,c1) -> 
      (well_typed_expression e pi) 
	&& (expression_type e pi = Boolexp) 
	&& (well_typed_command c1 pi)
  | Skip  -> true
;;



(* ---------------------------------------------------------------------- *)
(* typing programs                                                        *)
(* ---------------------------------------------------------------------- *)


let rec well_typed_program (Prog(d,c)) = 
   let pi = (declaration_type d pi0) in
   if ((well_typed_declaration d pi0) && (well_typed_command c pi)) then
   true
   else false
;;
