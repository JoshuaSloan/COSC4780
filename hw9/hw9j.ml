(* Josh Sloan *)

(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2015
*)

(* base code for HW 9 *)

(* Implements type structures and records as described in Schmidt text. *)


(*------ IMPORTANT   --------- *)
(* LOOK FOR "replace_this_env_store_pair_with_code" FOR WHERE YOU NEED
   TO ADD CODE. *)




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
    | h::t -> if (member h m) then false else (disjoint t m)
;;


(* ====================================================================== *)
(* ABSTRACT SYNTAX                                                        *)
(* ====================================================================== *)

(*----------------------------------------------------------------------- *)
(* Indentifier Expressions                                                *)
(*----------------------------------------------------------------------  *)

type identifier = Id of string | XId of identifier * string ;;

(* lastId -- returns the rightmost string in an identifier *)
let lastId id = 
  match id with 
      Id i -> i
    | XId (id,i) -> i
;;

let rec string_of_identifier id = 
  match id with
      Id s -> s
    | XId (i,s) -> string_of_identifier i ^ "." ^ s
;;

(* test cases *)

(* string_of_identifier (XId (Id "Graphics", "origin"));; *)
(* string_of_identifier (XId (XId (Id "Objects","Graphics"), "origin"));;    *)



(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)

type loc =  Loc of int;;

let string_of_loc (Loc i) = "Loc" ^ (string_of_int i);;


(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)

type expression =
    Num of int
  | Deref of identifier
  | Plus of expression * expression
  | Not of expression
  | Eq of expression * expression
  | Funcall of identifier
;;

let rec string_of_expression e =
  match e with 
    Num (i) -> "Num " ^ (string_of_int i)
  | Deref (al) -> "Deref " ^ (string_of_identifier al)
  | Plus (e1,e2) -> 
      "Plus (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e2) ^ ")"
  | Not (e1) -> "Not " ^ (string_of_expression e1)
  | Eq (e1,e2) -> 
      "Eq  (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e2) ^ ")"
  | Funcall s -> "Funcall " ^ (string_of_identifier s)
;;


(* ---------------------------------------------------------------------- *)
(* commands                                                               *)
(* ---------------------------------------------------------------------- *)

type command = 
    Assign of identifier * expression
  | Seq of command * command
  | Ite of expression * command * command
  | While of expression * command
  | Skip
  | Call of identifier
;;

let rec string_of_command c = 
  match c with 
    Assign (al,e) -> 
      "Assign (" ^ (string_of_identifier al) ^ ", " ^ (string_of_expression e) ^ ")"
  | Seq (c1,c2) -> "Seq (" ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | Ite (e,c1,c2) -> "Ite (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | While (e,c1) -> "While (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ")"
  | Skip  -> "Skip "
  | Call s -> "Call " ^ (string_of_identifier s)
;;

(* ---------------------------------------------------------------------- *)
(* declarations and type structures                                       *)
(* ---------------------------------------------------------------------- *)

type declaration = 
    Var of string * type_structure
  | TClass of string * type_structure
  | Const of string * expression
  | Fun of string * expression 
  | Proc of string * command 
  | Comma of declaration * declaration 
  | Semi of declaration * declaration
and type_structure =
    Newint
  | Record of declaration
  | X of identifier
;;


let rec string_of_declaration d = 
  match d with
      Var (i,t) -> "Var(" ^ i ^ ", " ^ string_of_type_structure t ^ ")"
    | TClass (i,t) -> "TClass(" ^ i ^ ", " ^ string_of_type_structure t ^ ")"
    | Const (i,k) -> "Const(" ^  i ^ ", " ^ string_of_expression  k ^ ")"
    | Fun (i, e) -> "Fun(" ^ i ^ ", " ^ string_of_expression  e ^ ")"
    | Proc (i, c) -> "Proc(" ^ i ^ ", " ^ string_of_command c ^ ")"
    | Comma (d1,d2) -> "Comma(" ^ string_of_declaration d1 ^ ", " ^ string_of_declaration d2 ^ ")"
    | Semi (d1, d2) -> "Semi(" ^ string_of_declaration d1 ^ ", " ^ string_of_declaration d2 ^ ")"
and string_of_type_structure t  =
  match t with 
      Newint -> "Newint"
    | Record d -> "Record(" ^ string_of_declaration d ^ ")"
    | X id ->  (string_of_identifier id)
;;


(* test cases *)
Newint;;
Record (Var ("X",Newint));;
Record (Semi (Var ("X",Newint),Fun ("f", Deref  (Id "X"))));;
TClass ("R",Record (Semi (Var ("X",Newint),Fun ("f", Deref (Id "X")))));;
Semi (TClass ("R",Record (Semi (Var ("X",Newint),Fun ("f", Deref (Id "X"))))),
      Var( "x",  X (Id "R")));;


(* ---------------------------------------------------------------------- *)
(* programs                                                               *)
(* ---------------------------------------------------------------------- *)

type program = Prog of declaration * command;;

(* ====================================================================== *)
(* TYPE CHECKING                                                          *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* see pg. 56 of Schmitt                                                  *)
(* Delta -> delta here : groups intlocs with type assignments,            *)
(* Pi -> type_assigments                                                  *)
(* Theta -> types                                                         *)
(* ---------------------------------------------------------------------- *)

type types =  
    Int 
  | Bool 
  | Intexp  
  | Boolexp  
  | Command  
  | Type of delta  
  | Class of delta 
(*   | Dec of type_assignment   -- we don't need declarations until we add modules  *)
and delta =  
    Intloc  
  | Pi of type_assignment
and  type_assignment =  
    TA of ((string * types) list) 
;; 


let rec  string_of_types t =
  match t with
      Int -> "int"
    | Bool -> "bool"
    | Intexp -> "intexp"
    | Boolexp -> "boolexp"
    | Command -> "comm"
    | Type d -> string_of_delta d
    | Class d -> "class " ^ string_of_delta d
(*    | Dec pi -> "dec " ^ string_of_type_assignment pi  *)
and string_of_delta d = 
  match d with
      Intloc -> "intloc"
    | Pi p -> string_of_type_assignment p
and string_of_type_assignment (TA m) =
  let rec sota m = 
    match m with 
	[] -> ""
      | (id,ty)::[] -> id ^ ": " ^ (string_of_types ty)
      | (id,ty)::m' -> id ^ ": " ^ (string_of_types ty) ^ "; " ^ sota m'
  in
    "[" ^ sota m ^ "]"
;;

let print_ta (TA f) = 
  let rec pta f =
    match f with
	[] -> ()
      | (name,ty)::t -> 
	  print_string name;
	  print_string ":"; 
          print_string (string_of_types ty);
          (if not (t = []) then 
	     print_string ", "
	   else
	     ());
	  pta  t
  in
    print_string "{";
    pta f;
    print_string "}"
;;



(* ---------------------------------------------------------------------- *)
(* Operations on Type Assignments                                         *)
(* ---------------------------------------------------------------------- *)


exception TypeError of expression ;;

let update_ta (i,v) (TA f) = TA ((i,v)::f);;

let rec lookup_ta  id (TA f) = 
  match f with
      [] -> 
	(raise (Failure ("no such type assignment: " ^ id))  : types)
    | (name,ty)::t -> if id = name then ty else (lookup_ta  id (TA t))
;;

let names_of_type_assignment (TA f) = map fst f;;

let union_ta (TA l) (TA m) = 
  if disjoint l m then 
    TA (l @ m)
  else
    raise (Failure "union_ta: not disjoint.")
;;

let bar_union_ta (TA m) (TA n) = TA (n @ m);;

(* pi0 is the empty type assignment *)
let pi0  = TA [] ;;



(* ---------------------------------------------------------------------- *)
(* typing identifiers                                                     *)
(* ---------------------------------------------------------------------- *)

let rec type_of_identifier id pi = 
  match id with
      Id s -> lookup_ta s pi
    | XId (id,s) ->
	match type_of_identifier id pi with
	    Class (Pi pi1) -> lookup_ta s pi1
	  | Type (Pi pi1) -> lookup_ta s pi1

	  | _ -> raise (Failure "type_of_identifier: Bad type.")
;;
	  
let well_typed_identifier id pi =
  try   
    let _ = type_of_identifier id pi in
      true
  with
      _ -> false
;;


(* ---------------------------------------------------------------------- *)
(* in locations                                                       *)
(* ---------------------------------------------------------------------- *)

let well_typed_loc id pi = 
  type_of_identifier id pi = Type Intloc
;;

(* ---------------------------------------------------------------------- *)
(* typing expressions                                                     *)
(* ---------------------------------------------------------------------- *)

let rec expression_type e pi =
  match e with 
      Num (i) -> Intexp
    | Deref (al) -> 
	if (well_typed_loc al pi) then Intexp else raise (TypeError e)
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
    | Funcall x -> type_of_identifier x pi

;;

let well_typed_expression e pi = 
  try (let _ = expression_type e pi in true ) with TypeError e -> false 
;;

(* ---------------------------------------------------------------------- *)
(* typing declarations                                                    *)
(* ---------------------------------------------------------------------- *)

let rec declaration_type d pi =
  match d with
      Var (i,t) ->
	(match (type_structure_type t pi) with
	     Class t_type -> 
	       update_ta (i, Type t_type ) pi0
	   | _ -> raise (Failure "declaration_type: bad var declaration.") )
    | TClass (i,t) ->
	(match (type_structure_type t pi) with 
	     Class dc -> update_ta (i, Class dc) pi0
	   | _ -> raise (Failure ("declaration_type: " ^ (string_of_type_structure t) ^ " not a type class.")))
    | Const (id,e) -> update_ta (id, expression_type e pi) pi0
    | Fun (id, body) -> update_ta (id, expression_type body pi) pi0
    | Proc (id, body) -> update_ta (id, Command) pi0
    | Comma (d1,d2) -> union_ta (declaration_type d1 pi) (declaration_type d2 pi)
    | Semi (d1, d2) -> 
	let pi1 = (declaration_type d1 pi) in
	let pi2 = declaration_type d2 (bar_union_ta pi pi1) in 
	  bar_union_ta pi1 pi2
and type_structure_type t pi =
  match t with 
      Newint -> Class Intloc
    | Record d -> 
	let pi1 = declaration_type d pi in
	  Class (Pi pi1)
    | X id -> type_of_identifier id pi
;;

(* a few tests  *)
(* declaration_type (Var ("x",Newint)) pi0;; *)
(* declaration_type (Semi(Var ("x",Newint),Fun("f",Deref (Id "x")))) pi0;; *)
(* declaration_type (Semi(Var ("x",Newint),Fun("f",Eq(Deref (Id "x"),Num 1)))) pi0;; *)
(* declaration_type (TClass ("M", Newint)) pi0;; *)
(* declaration_type (Semi(TClass ("M", Newint), Var ("x", X(Id "M")))) pi0;; *)
(* declaration_type (TClass ("R", Record (Var ("x",Newint)))) pi0 ;; *)
(* declaration_type (Semi(TClass ("R", Record (Var ("x",Newint))), Var ("r",X(Id "R")))) pi0 ;; *)


let well_typed_declaration d pi =
  try (let _ = declaration_type d pi in true ) with  _ -> false  
;;
  
let well_typed_type_structure ts pi = 
  try (let _ = type_structure_type ts pi in true ) with _ -> false  
;;



(* ---------------------------------------------------------------------- *)
(* typing commands                                                        *)
(* ---------------------------------------------------------------------- *)

let rec well_typed_command c pi =
  match c with 
      Assign (id,e) -> 
	(well_typed_loc id pi) 
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
    | Call x -> type_of_identifier x pi = Command
;;


(* ---------------------------------------------------------------------- *)
(* typing programs                                                        *)
(* ---------------------------------------------------------------------- *)

let well_typed_program (Prog(d,c)) =
  try 
    let pi = declaration_type d pi0 in
      well_typed_command c pi
  with
      _ -> false
;;



(* ====================================================================== *)
(* SEMANTICS                                                              *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* stores                                                                 *)
(* ---------------------------------------------------------------------- *)

type store = Store of  (int * (loc -> int)) ;;
let size (Store (i,f)) =  i;;
let mem (Store (i,f)) = f ;;

let lookup ((Loc i) , s) =
  if (i > size s) || (i <= 0) then
    raise (Failure "lookup: address out of range")
  else
    (mem s) (Loc i)
 ;;

let update_store (Loc i, v, s) =
  if i > (size s) || (i < 0) then
    raise (Failure "update_store: address out of range")
  else
    Store (size s, fun x -> if ((Loc i) = x) then v else ((mem s) x))
;;

let allocate  (Store (sz,mem)) = 
  let init = 0 in
  let nsz = sz + 1 in
    (Loc nsz, update_store (Loc nsz, init, Store (nsz, mem)))
;;

let free i (Store (sz,mem)) = 
   if (i < sz && i >= 0) then
     Store(i,mem) 
   else 
     raise (Failure "free: new size must be between 0 and current size.")
;;

(* s0 is the empty store initialized to zero *)
let s0  = Store (0, (fun x -> raise (Failure "addrress out of range."))) ;;


let print_store_to i (Store (sz,f)) = 
  let rec ps j = 
    if (j <= i) && (j <= sz) then
      (print_string "Loc";
       print_int j ;
       print_string ":";
       print_int (f (Loc j)); 
       print_string (if (j < sz) then ", " else "");
       ps (j + 1))
    else
      ()
  in
 ps 1
;;

let print_store s = print_string "<"; print_store_to (size s) s; print_string ">\n";;

(* ---------------------------------------------------------------------- *)
(* environments                                                           *)
(* ---------------------------------------------------------------------- *)


type env = { int : string -> int;
             bool : string -> bool;
	     intexp : string -> store -> int;
	     boolexp: string -> store -> bool;
             command: string -> store -> store;
	     intloc : string -> loc;
	     pi : string -> env;
	     intloc_class : string -> store -> (loc * store);
	     pi_class : string -> store -> (env * store)
	   }
;;

let update_int_env (i,v) e = 
  {int = update (i,v) e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp =  e.boolexp;
   command = e.command;
   intloc = e.intloc;
   pi = e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;


let update_bool_env (i,v) e = 
  {int = e.int;
   bool = update (i,v) e.bool;
   intexp = e.intexp;
   boolexp =  e.boolexp;
   command = e.command;
   intloc = e.intloc;
   pi = e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;



let update_intexp_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = update (i,v) e.intexp;
   boolexp =  e.boolexp;
   command = e.command;
   intloc = e.intloc;
   pi = e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;
let update_boolexp_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = update (i,v) e.boolexp;
   command = e.command;
   intloc = e.intloc;
   pi = e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;
let update_command_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command = update (i,v) e.command;
   intloc = e.intloc;
   pi = e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;

let update_intloc_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc = update (i,v) e.intloc;
   pi = e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;

let update_pi_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc =  e.intloc;
   pi = update (i,v) e.pi;
   intloc_class = e.intloc_class;
   pi_class = e.pi_class
  }
;;

let update_intloc_class_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc =  e.intloc;
   pi = e.pi;
   intloc_class = update (i,v) e.intloc_class;
   pi_class = e.pi_class
  }
;;

let update_pi_class_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc =  e.intloc;
   pi = e.pi;
   intloc_class =  e.intloc_class;
   pi_class = update (i,v) e.pi_class
  }
;;

let union_env e1 e2  = 
  {int = (fun i  -> try (e1.int i) with _ -> e2.int i );
   bool = (fun i  -> try (e1.bool i ) with _ -> e2.bool i );
    intexp = (fun i s -> try (e1.intexp i s) with _ -> e2.intexp i s);
    boolexp = (fun i s -> try (e1.boolexp i s) with _ -> e2.boolexp i s);
    command = (fun i s -> try (e1.command i s) with _ -> e2.command i s);
    intloc = (fun i -> try (e1.intloc i) with _ -> e2.intloc i);
    pi =  (fun i  -> try (e1.pi i ) with _ -> e2.pi i);
    intloc_class = (fun i s  -> try (e1.intloc_class i s) with _ -> e2.intloc_class i s);
    pi_class = (fun i s -> try (e1.pi_class i s ) with _ ->  e2.pi_class i s)
  }
;;

(* env0 is the empty environment - it rasies failure for any lookup. *)
let env0 = 
  {int = (fun i -> raise (Failure (i ^ " not in int environment.")): string -> int);
   bool = (fun i -> raise (Failure (i ^ " not in bool environment.")): string -> bool);
   intexp = (fun i s -> raise (Failure (i ^ " not in intexp environment.")): string -> store -> int);
   boolexp = (fun i s -> raise (Failure (i ^ " not in boolexp environment.")): string -> store -> bool);
   command = (fun i s -> raise (Failure (i ^ " not in command environment.")): string -> store -> store);
   intloc = (fun i -> raise (Failure (i ^ " not in intloc environment.")): string -> loc);
   pi =  (fun i  -> raise (Failure (i ^ " not in pi environment.")): string -> env) ;
   intloc_class = (fun i s  -> raise (Failure (i ^ " not in intloc_class environment.")): string -> store -> (loc * store));
   pi_class = (fun i s -> raise (Failure (i ^ " not in pi_class environment.")): string -> store -> (env * store));
  }
;;

let print_env  pi e s  =
  let rec p_env (TA alst) e s inclass =
    let rec penv f  =
      match f with
	  [] -> ()
	| (name,ty)::t -> 
	    print_string name;
	    print_string ":"; 
	    (match ty with
	       | Int -> print_string "int "; print_int (e.int name)	 
	       | Bool -> print_string "bool "; print_string (string_of_bool (e.bool name))
	       | Intexp -> print_string "intexp "; print_int (e.intexp name s)
	       | Boolexp -> print_string "boolexp "; print_string (string_of_bool (e.boolexp name s))
	       | Command ->  print_string "comm "; print_store (e.command name s)
	       | Type Intloc -> 
		   if inclass then
		     print_string "Newint"
		   else 
		     print_string ("intloc " ^ (string_of_loc (e.intloc name)))
	       | Type (Pi pi1) -> 
		   print_string "rec"; 
		   p_env pi1 (e.pi name) s false
	       | Class Intloc -> print_string "Newint"
               | Class (Pi pi1) -> 
		   let (e1,s1) = e.pi_class name s in
		     print_string "class ";
                     p_env pi1 e1 s1 true
	    );
            (if not (t = []) then 
	       print_string ", "
	     else
	       ());
	    penv t
    in
      print_string "{";
      penv alst;
      print_string "}"
  in
    p_env pi e s false
;;

(* ---------------------------------------------------------------------- *)
(* meaning of locations                                                   *)
(* ---------------------------------------------------------------------- *)

let meaning_of_loc (Loc i) e = Loc i ;;

(* ---------------------------------------------------------------------- *)
(* meaning of identifiers                                                 *)
(* ---------------------------------------------------------------------- *)


(* this function recurses through nested environments using identifiers to naviagte
 to find the enviroment to lookup an identifier in *)

let rec get_lookup_env id pi env err = 
  match id with
      Id i -> env
    | XId (id,i) ->
	(match (type_of_identifier id pi) with
	     Type (Pi pi') ->
	       (get_lookup_env id (bar_union_ta pi' pi) env err).pi (lastId id)
	   | _ -> raise (Failure ("meaning_of_" ^ err ^ ": (-> get_lookup_env) - bad record id.")))
;;

let meaning_of_int_id id pi env =
  let env' = get_lookup_env id pi env "int_id" in
    env'.int (lastId id)
;;

let meaning_of_bool_id id pi env =
  let env' = get_lookup_env id pi env "bool_id" in
    env'.bool (lastId id)
;;

let meaning_of_intexp_id id pi env = 
  let env' = get_lookup_env id pi env "intexp_id" in
    env'.intexp (lastId id)
;;

let meaning_of_boolexp_id id pi env = 
  let env' = get_lookup_env id pi env "boolexp_id" in
    env'.boolexp (lastId id)
;;

let meaning_of_command_id id pi env = 
  let env' = get_lookup_env id pi env "command_id" in
    env'.command (lastId id)
;;

let meaning_of_intloc_id id pi env = 
  let env' = get_lookup_env id pi env "intloc_id" in
    env'.intloc (lastId id)
;;

let meaning_of_pi_id id pi env = 
  let env' = get_lookup_env id pi env "pi_id" in
    env'.pi (lastId id)
;;

let meaning_of_intloc_class_id id pi env = 
  let env' = get_lookup_env id pi env "intloc_class_id" in
    env'.intloc_class (lastId id)
;;

let meaning_of_pi_class_id id pi env = 
  let env' = get_lookup_env id pi env "pi_class_id" in
    env'.pi_class (lastId id)
;;

(* ---------------------------------------------------------------------- *)
(* meaning of expresions --                                               *)
(* ---------------------------------------------------------------------- *)

let rec meaning_of_int_expression e pi env s =
  match e with 
      Num i ->  i
    | Deref al -> lookup (meaning_of_intloc_id al pi env, s)
    | Plus (e1,e2) -> 
	(meaning_of_int_expression e1 pi env s) 
	+ (meaning_of_int_expression e2 pi env s)
    | Funcall id ->  meaning_of_intexp_id id pi env s
    | _ -> 
	let msg = "meaning_of_int_expression: " ^ (string_of_expression e)  in
	  raise (Failure msg)
;;

let rec meaning_of_bool_expression e pi env s =
  match e with 
      Not e1 -> not (meaning_of_bool_expression e1 pi env s)
    | Eq (e1,e2) -> 
	(match (expression_type e1 pi, expression_type e2 pi) with
	     (Boolexp, Boolexp) -> 
	       (meaning_of_bool_expression e1 pi env s)
	       = (meaning_of_bool_expression e2 pi env s)
	   | (Intexp, Intexp) -> 
	       (meaning_of_int_expression e1 pi env s)
	       = (meaning_of_int_expression e2 pi env s)
	   | _ -> 
	       let msg = "meaning_of_bool_expression: (err 1) " 
		 ^ (string_of_expression e) ^ "." 
	       in
		 raise (Failure msg))
    | Funcall id -> meaning_of_boolexp_id id pi env s
    | _ -> 
	let msg = "meaning_of_bool_expression: (err 2) " 
	  ^ (string_of_expression e) ^ "." 
	in
	  raise (Failure msg)
;;

(* ---------------------------------------------------------------------- *)
(* meaning of commands                                                    *)
(* ---------------------------------------------------------------------- *)

let rec meaning_of_command c pi env s =
  match c with 
    Assign (al,e) -> 
      update_store (meaning_of_intloc_id al pi env, 
		    meaning_of_int_expression e pi env s, 
		    s)
  | Seq (c1,c2) -> 
      meaning_of_command c2 pi env (meaning_of_command c1 pi env s)
  | Ite (e,c1,c2) -> 
      if (meaning_of_bool_expression e pi env s) then 
	(meaning_of_command c1 pi env s)
      else 
	(meaning_of_command c2 pi env s)
  | While (e,c1) -> 
      (let rec w s = 
	if (meaning_of_bool_expression e pi env s) then
	  w (meaning_of_command c1 pi env s)
	else s
      in
        w s)
  | Skip  -> s
  | Call id -> meaning_of_command_id id pi env s
;;

(* ---------------------------------------------------------------------- *)
(* meaning of type structures and meaning of declarations                 *)
(* ---------------------------------------------------------------------- *)

(* for classifying type_structures *)

let is_intloc_class ts pi = 
  match ts with
      Newint -> true
    | Record _ -> false
    | X id -> match type_of_identifier id pi with
	  Class Intloc -> true
	| _ -> false
;;

let is_pi_class ts pi = 
  match ts with
      Newint -> false
    | Record _ -> true
    | X id -> match type_of_identifier id pi with
	  Class (Pi _ )  -> true
	| _ -> false
;;


(* meaning_of_declaration :: declaration -> type_assignment -> env -> store -> env * store = <fun> *)

let replace_this_env_store_pair_with_code = (env0,s0) ;;
let rec meaning_of_declaration d pi env s  = 
    match d with
	Var (id, ts) -> 
	  if is_intloc_class ts pi then 
            let (l,s') = meaning_of_newint ts pi env s in
            ((update_intloc_env (id,l) env0), s')

	  else if is_pi_class ts pi then
            let (e,s') = meaning_of_record ts pi env s in
            ((update_pi_env (id,e) env0), s')

	  else
	    raise (Failure "meaning_of_declaration: bad class.")
      | TClass(name,ts) ->
	  if is_intloc_class ts pi then 
            let f = meaning_of_newint ts pi env in
            ((update_intloc_class_env (name,f) env0), s)

	  else if is_pi_class ts pi then
            let f = meaning_of_record ts pi env in
            ((update_pi_class_env (name,f) env0), s)

	  else
	    raise (Failure "meaning_of_declaration: bad class.")
      | Const (id, e) -> 
	  (match (expression_type e pi) with
	       Intexp -> (update_intexp_env (id,  fun s' -> meaning_of_int_expression e pi env s) env0, s) 
	     | Boolexp -> (update_boolexp_env (id,  fun s' -> meaning_of_bool_expression e pi env s) env0, s) 
	     | _ -> raise (Failure ("meaning_of_declaration: bad const.")))

      | Fun (id, e) -> 
	  (match (expression_type e pi) with
	       Intexp -> (update_intexp_env (id,  fun s' -> meaning_of_int_expression e pi env s') env0, s) 
	     | Boolexp -> (update_boolexp_env (id,  fun s' -> meaning_of_bool_expression e pi env s') env0, s) 
	     | _ -> raise (Failure ("meaning_of_declaration: bad fun.")))

      | Proc (id, c) -> (update_command_env (id, fun s' -> meaning_of_command c pi env s') env0, s)

      | Semi (d1,d2) -> 
	  let (env1,s1)  = meaning_of_declaration d1 pi env s in
	  let (env2,s2) = meaning_of_declaration d2 (bar_union_ta pi (declaration_type d1 pi))(union_env env env1) s1
	  in (union_env env1 env2, s2)

      | Comma (d1,d2) -> 
	  let (env1,s1)  = meaning_of_declaration d1 pi env s in
	  let (env2,s2) = meaning_of_declaration d2 pi env s1
	  in (union_env env1 env2, s2)

and meaning_of_newint t pi env s = 
  match t with
      Newint -> allocate s 
    | Record _  -> raise (Failure "meaning_of_newint: records not intloc class.")
    | X id -> meaning_of_intloc_class_id id pi env s 

and meaning_of_record t pi env s = 
    match t with
      Newint -> raise (Failure "meaning_of_pi_class: Newint is not a pi class.")
    | Record d ->  meaning_of_declaration d pi env s 
    | X id -> meaning_of_pi_class_id id pi env s
;;


(* ---------------------------------------------------------------------- *)
(* meaning of programs                                                    *)
(* ---------------------------------------------------------------------- *)

let meaning_of_program (Prog (d,c)) s =
  let pi = declaration_type d pi0 in
  let (env,s) = meaning_of_declaration d pi env0 s in
    meaning_of_command c pi env s
;;




