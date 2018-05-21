(* Josh Sloan *)

(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2012
*)

(* Code base for HW 10 -- implementing a call by name evaluator.  you
   need to implement the case for procedure declarations in
   meaning_of_declaration.  The equations are on page 80 of Schmidt.
*)


(* ====================================================================== *)
(* UTILITIES                                                              *)
(* ====================================================================== *)

let update_fn (x,v) f = fun y -> if x = y then v else f y ;;

let rec map f l = 
  match l with 
      [] -> []
    | h::t -> (f h)::(map f t)
;;

let member = List.mem;;

let rec disjoint l m = 
  match l with
      [] -> true
    | h::t -> if (member h m) then false else (disjoint t m)
;;

let disjoint_union l m = 
 if disjoint l m then 
   (l @ m)
 else
   raise (Failure "disjoint_union: not disjoint.")
;;

let rec remove_all x l = 
  match l with
      [] -> []
    | h::t -> 
	let t' = remove_all x t in 
          if h = x then t' else h::t'
;;

let rec unique l = 
  match l with 
      [] -> []
    | h::t -> h:: (unique (remove_all h t))
;;


(* ====================================================================== *)
(* ABSTRACT SYNTAX                                                        *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* semantic addresss -- used to index the store                           *)
(*   -- we used to have only loc, now we have type addresss and type loc  *)
(*      address is like the old loc and loc now just the name of a        *)
(*      variable -- which we need to look up in the loc environment       *)
(* ---------------------------------------------------------------------- *)

type address =  Address of int;;

let string_of_address l = 
  match l  with
      Address i -> "Address " ^ (string_of_int i)
;;

(* ---------------------------------------------------------------------- *)
(* syntactic locations -- just identifiers now                            *)
(* ---------------------------------------------------------------------- *)

type loc =  I of string;;

let string_of_loc (I s) = s ;;

(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)

type expression =
    Num of int
  | Deref of loc
  | Plus of expression * expression
  | Not of expression
  | Eq of expression * expression
  | Id of string                            (* Id "f" is the syntax of an expression referencing the function "f" *)
;;

let rec string_of_expression e =
  match e with 
    Num (i) -> "Num " ^ (string_of_int i)
  | Deref (al) -> "Deref " ^ (string_of_loc al)
  | Plus (e1,e2) -> "Plus (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e1) ^ ")"
  | Not (e1) -> "Not " ^ (string_of_expression e1)
  | Eq (e1,e2) -> "Eq  (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e1) ^ ")"
  | Id s -> "Id " ^ s
;;




(* ---------------------------------------------------------------------- *)
(* types -- used in declarations                                          *)
(* ---------------------------------------------------------------------- *)

type expression_types = Int |  Bool ;;

let string_of_expression_types t = 
  match t with
      Int -> "Int"
    | Bool -> "Bool"
;;

type types = 
   TauExp of expression_types 
 | Intloc 
 | Comm 
 | Arrow of types * types ;;       (* this is for creating types like (TauExp(Int) -> Comm)  *)

let rec string_of_types t = 
  match t with 
    TauExp e -> "TauExp " ^ (string_of_expression_types e)
  | Intloc -> "Intloc"
  | Comm -> "Comm"
  | Arrow (t1,t2) -> "(" ^ (string_of_types t1) ^ " -> " ^ (string_of_types t2) ^ ")"
;;

(* ---------------------------------------------------------------------- *)
(* declarations and commands                                                           *)
(* ---------------------------------------------------------------------- *)

type  command = 
    Assign of loc * expression
  | Seq of command * command
  | Ite of expression * command * command
  | While of expression * command
  | Skip
  | Call of string * expression                  (* Call ("P",e) is the syntax for calling procedure "P" with argument e *)
;;

let rec string_of_command c = 
  match c with 
    Assign (al,e) -> "Assign (" ^ (string_of_loc al) ^ ", " ^ (string_of_expression e) ^ ")"
  | Seq (c1,c2) -> "Seq (" ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | Ite (e,c1,c2) -> "Ite (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | While (e,c1) -> "While (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ")"
  | Skip  -> "Skip "
  | Call(name,arg) -> "Call " ^ name ^ "(" ^ (string_of_expression arg) ^ ")"
;;

type declaration = 
    Var of string
  | Fun of string * expression                               (* non parameterized function declaration *)
(* parameterized  procedure declarations *)
(* Proc I1(I2:T) = C   appears as  Proc(I1,I2,T,C)  *)
  | Proc of string * string * types * command  
  | Comma of declaration * declaration 
  | Semi of declaration * declaration
;;

let rec string_of_declaration d = 
  match d with
    Var id -> "Var " ^ id ^ ": newint"
  | Fun (name, e) -> "Fun " ^ name ^ " = " ^ (string_of_expression e)
  | Proc(i1, i2, t, c) -> "Proc " ^ i1 ^ "(" ^ i2 ^ ":" ^ (string_of_types t) ^ ") = " ^ (string_of_command c)
  | Comma(d1,d2) -> (string_of_declaration d1) ^ ", " ^ (string_of_declaration d2)
  | Semi(d1,d2) -> (string_of_declaration d1) ^ "; " ^ (string_of_declaration d2)
;;

(* ---------------------------------------------------------------------- *)
(* programs                                                               *)
(* ---------------------------------------------------------------------- *)

type program = Prog of declaration * command;;
let declaration_of_program (Prog (d,c)) = d;;
let command_of_program (Prog (d,c)) = c;;

(* ====================================================================== *)
(* TYPE CHECKING                                                          *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* type assignments                                                       *)
(* ---------------------------------------------------------------------- *)

exception TypeError of expression ;;

type type_assignment = TA of ((string * types) list) ;;

let update_ta (i,v) (TA f) = TA ((i,v)::(List.filter (fun (j,_) -> not (j = i)) f));;

let rec type_of_id (TA f) id = 
  match f with
      [] -> (raise (Failure ("no such type assignment: " ^ id))  : types)
    | h::t -> if id = (fst h) then (snd h) else (type_of_id (TA t) id)
;;

let is_id id pi = try (let _ = type_of_id pi id in true) with _ -> false;;

let names_of_type_assignment (TA f) = map fst f;;

let union_ta (TA p1) (TA p2) = 
  if disjoint (map fst p1) (map fst p2) then
     TA (p1 @ p2) 
  else
    raise (Failure "union_ta: bad union - not disjoint.")
;; 

let bar_union_ta (TA p1) (TA p2) = 
  let p2_names = map fst p2 in
  let p1' = List.filter (fun (i,_) -> not (member i p2_names)) p1 in
   TA (p2 @ p1')
;;

(* pi0 is the empty type assignment *)
let pi0  = TA [] ;;

let print_ta (TA f) =
  let rec pta f =
    match f with
	[] -> ()
      | (i,ty)::t -> 
	  (print_string i;
	   print_string ":"; 
           print_string (string_of_types ty);
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
(* typing locations -- different from the type of addresses               *)
(* ---------------------------------------------------------------------- *)

let well_typed_loc (I name) pi = 
  (type_of_id pi name =  Intloc)
;;

(* ---------------------------------------------------------------------- *)
(* typing expressions                                                     *)
(* ---------------------------------------------------------------------- *)


let rec expression_type e pi =
  match e with 
    Num (i) -> Int
  | Deref (name) -> 
      if (well_typed_loc name pi) then Int else raise (TypeError e)
  | Plus (e1,e2) -> 
      if ((expression_type e1 pi = Int) && (expression_type e2 pi = Int)) then 
	Int
      else
	raise(TypeError e)
  | Not (e1) ->   
      if (expression_type e1 pi = Bool) then Bool else raise(TypeError e)
  | Eq (e1,e2) -> 
      let e1t = (expression_type e1 pi) in
      let e2t = (expression_type e2 pi) in
      if (e1t = e2t) then Bool else raise(TypeError e)
  | Id name ->
      match (type_of_id pi name) with
	  TauExp et -> et
	| _ -> raise (TypeError e)
;;

let well_typed_expression e pi = 
  try (let et = expression_type e pi in (true || et = Bool) ) with TypeError e -> false 
;;

(* ---------------------------------------------------------------------- *)
(* typing commands and declarations                                                    *)
(* ---------------------------------------------------------------------- *)

let rec well_typed_command c pi =
  match c with 
      Assign (name,e) -> 
	(well_typed_loc name pi) 
	&& (well_typed_expression e pi) 
	&& (expression_type e pi = Int)
    | Seq (c1,c2) -> (well_typed_command c1 pi) && (well_typed_command c2 pi)
    | Ite (e,c1,c2) -> 
	(well_typed_expression e pi) 
	&& (expression_type e pi =  Bool) 
	&& (well_typed_command c1 pi) 
	&& (well_typed_command c2 pi)
    | While (e,c1) -> 
	(well_typed_expression e pi) 
	&& (expression_type e pi =  Bool) 
	&& (well_typed_command c1 pi)
    | Skip  -> true
    | Call (p,e) -> 
	(match (type_of_id pi p) with
             Arrow(t1,t2) -> 
               (well_typed_expression e pi) 
               && (t1 = TauExp (expression_type e pi))
	   | _ -> false)

;;

let rec declaration_type d pi =
  match d with
      Var id ->  update_ta (id, Intloc) pi0
    | Fun (id, body) -> 
	update_ta (id, TauExp (expression_type body pi)) pi0
    | Proc(i1, i2, t, body) -> 
	(match t with 
	     (TauExp t') -> 	
	       if (well_typed_command body  (bar_union_ta pi  (TA [i2, t])))then
		 update_ta (i1, Arrow (t, Comm)) pi0
	       else
		 raise (Failure "declaration_type: badly typed procedure body." )
	   | _ -> raise (Failure "declaration_type: bad argument type." ))
    | Comma (d1,d2) -> 
	union_ta (declaration_type d1 pi) (declaration_type d2 pi)
    | Semi (d1, d2) -> 
	let pi1 = (declaration_type d1 pi) in
	let pi2 = declaration_type d2 (union_ta pi pi1) in 
	  union_ta pi1 pi2
and well_typed_declaration d pi =
  try (let _ = declaration_type d pi in true) with  _ -> false  
;;


(* ====================================================================== *)
(* SEMANTICS                                                              *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* stores -- modified to include a bound on the store size                *)
(* ---------------------------------------------------------------------- *)

type store = Store of  (int * (address -> int)) ;;

let new_store size init = Store (size, fun i -> init);;
let size (Store (i,f)) =  i;;
let mem (Store (i,f)) = f ;;

let lookup ((Address i) , s) =
  if (i > size s) || (i <= 0) then
    0
  else
    (mem s) (Address i)
 ;;

let update (Address i, v, s) =
  if i > (size s) || (i < 0) then
    s
  else
    Store (size s, update_fn (Address i, v) (mem s))
;;

let allocate (Store (sz,f)) = (Address (sz + 1), Store (sz + 1, f));;

let free k (Store(sz,f)) = Store(k,f);;

(* print_store  prints the addresss of store s *)

let print_store (Store (i, f)) = 
  let rec ps j = 
    if j <= i then
      (print_string "Address";
       print_int j ;
       print_string ": ";
       print_int (f (Address j)); 
       print_string "\n";
       ps (j + 1))
    else
      ()
  in
 print_string "\n";
 ps 1
;;

let s0 = new_store 0 0;;


(* notice that print store now prints the entire store *)

let print_store  (Store (i,f)) = 
  let rec ps j = 
    if j <= i then
      (print_string "\n Address";
       print_int j ;
       print_string ": ";
       print_int (f (Address j)); 
       ps (j + 1))
    else
      ()
  in
 if i = 0 then 
   print_string "<>"
 else
   ps 1
;;



(* ---------------------------------------------------------------------- *)
(* environments                                                           *)
(* ---------------------------------------------------------------------- *)


type env = { int : string -> int;
             bool : string -> bool;
	     intexp : string -> store -> int;
	     boolexp : string -> store -> bool;
             command : string -> store -> store;
	     intloc : string -> address;
	     proc_int : string -> (store -> int) -> store -> store;  (* for processes parameterized by intexp *)
	     proc_bool : string -> (store -> bool) -> store -> store  (* for processes parameterized by boolexp *)
	   }
;;

let update_int_env (i,v) e = 
  {int = update_fn (i,v) e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp =  e.boolexp;
   command = e.command;
   intloc = e.intloc;
   proc_int = e.proc_int;
   proc_bool = e.proc_bool
  }
;;


let update_bool_env (i,v) e = 
  {int = e.int;
   bool = update_fn (i,v) e.bool;
   intexp = e.intexp;
   boolexp =  e.boolexp;
   command = e.command;
   intloc = e.intloc;
   proc_int = e.proc_int;
   proc_bool = e.proc_bool
  }
;;



let update_intexp_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = update_fn (i,v) e.intexp;
   boolexp =  e.boolexp;
   command = e.command;
   intloc = e.intloc;
   proc_int = e.proc_int;
   proc_bool = e.proc_bool
  }
;;
let update_boolexp_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = update_fn (i,v) e.boolexp;
   command = e.command;
   intloc = e.intloc;
   proc_int = e.proc_int;
   proc_bool = e.proc_bool
  }
;;
let update_command_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command = update_fn (i,v) e.command;
   intloc = e.intloc;
   proc_int = e.proc_int;
   proc_bool = e.proc_bool
  }
;;

let update_intloc_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc = update_fn (i,v) e.intloc;
   proc_int = e.proc_int;
   proc_bool = e.proc_bool
  }
;;

let update_proc_int_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc =  e.intloc;
   proc_int = update_fn (i,v) e.proc_int;
   proc_bool = e.proc_bool

  }
;;

let update_proc_bool_env (i, v) e = 
  {int = e.int;
   bool = e.bool;
   intexp = e.intexp;
   boolexp = e.boolexp;
   command =  e.command;
   intloc =  e.intloc;
   proc_int = e.proc_int;
   proc_bool = update_fn (i,v) e.proc_bool

  }
;;


let union_env e1 e2  = 
  {int = (fun i  -> try (e2.int i) with _ -> e1.int i );
   bool = (fun i  -> try (e2.bool i ) with _ -> e1.bool i );
    intexp = (fun i s -> try (e2.intexp i s) with _ -> e1.intexp i s);
    boolexp = (fun i s -> try (e2.boolexp i s) with _ -> e1.boolexp i s);
    command = (fun i s -> try (e2.command i s) with _ -> e1.command i s);
    intloc = (fun i -> try (e2.intloc i) with _ -> e1.intloc i);
    proc_int =  (fun i  -> try (e2.proc_int i ) with _ -> e1.proc_int i);
    proc_bool =  (fun i  -> try (e2.proc_bool i ) with _ -> e1.proc_bool i);
  }
;;

(* env0 is the empty environment - it rasies failure for any lookup. *)
let env0 = 
  {int = (fun i -> raise (Failure (i ^ " not in int environment.")));
   bool = (fun i -> raise (Failure (i ^ " not in bool environment.")));
   intexp = (fun i s -> raise (Failure (i ^ " not in intexp environment.")));
   boolexp = (fun i s -> raise (Failure (i ^ " not in boolexp environment.")));
   command = (fun i s -> raise (Failure (i ^ " not in command environment.")));
   intloc = (fun i -> raise (Failure (i ^ " not in intloc environment.")));
   proc_int = (fun i -> raise (Failure (i ^ " not in proc_int environment.")));
   proc_bool =  (fun i  -> raise (Failure (i ^ " not in proc_bool environment.")))

  }
;;

(* let print_env  pi e s  =
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
*)



(* print_env : type_assignment -> env -> store -> unit 
   given a type assignment and a store to evaluate the meaning functions  in, it is possible to reasonably print an environment 
   -- we iterate over all the names in the type assignment and then look them up in the proper environemnt uising 
      the type information and evaluate them in the store provided.
*)

let print_env (TA f) e s =
  let rec penv f =
    match f with
	[] -> ()
      | ((id,ty)::t) -> 
	  print_string id;
	  print_string ":"; 
	  (match ty with
	       (TauExp Int) -> print_string (string_of_int  (e.intexp id s))
	     | (TauExp Bool) -> 
		 if (e.boolexp id s) then 
		   (print_string "true")
		 else
		   (print_string "false")

	     | Intloc -> print_string (string_of_address (e.intloc id))

	     | Arrow (t1,t2) -> print_string "<fun>"
(*		 match t1 with 
		     (TauExp Int) -> try (e.proc_int 

*)	 
	     | Comm -> print_string "Comm"
	  );
	  
          (if not (t = []) then 
	     print_string ", "
	   else
	     ());
	  penv  t


  in
    print_string "{";
    penv f;
    print_string "}"
;;


(* ---------------------------------------------------------------------- *)
(* meaning of address and meaning of loc                                  *)
(* ---------------------------------------------------------------------- *)

let meaning_of_address (l : loc) = l ;;

let meaning_of_loc (I s) e  = e.intloc s;;

(* ---------------------------------------------------------------------- *)
(* meaning of expresions --                                               *)
(* ---------------------------------------------------------------------- *)

let rec meaning_of_int_expression e pi env s =
  match e with 
    Num i ->  i
  | Deref al -> lookup (meaning_of_loc al env, s)
  | Plus (e1,e2) -> (meaning_of_int_expression e1 pi env s) + (meaning_of_int_expression e2 pi env s)
  | Id v -> 
      if (type_of_id pi v = TauExp Int) then
	env.intexp v s
      else
	raise (Failure ((string_of_expression e) ^ " is not an int expression."))
  | _ -> raise (Failure ((string_of_expression e) ^ " is not an int expression."))
;;

let rec meaning_of_bool_expression e pi env s =
  match e with 
    Not e1 -> not (meaning_of_bool_expression e1 pi env s)
  | Eq (e1,e2) -> 
      (match (expression_type e1 pi, expression_type e2 pi) with
	(Bool, Bool) -> 
	  (meaning_of_bool_expression e1 pi env s)
	    = (meaning_of_bool_expression e2 pi env s)
      | (Int, Int) -> 
	  (meaning_of_int_expression e1 pi env s)
	    = (meaning_of_int_expression e2 pi env s)
      | _ -> raise (Failure ((string_of_expression e) ^ " is not a Boolean expression.")))
  | Id v -> 
      if (type_of_id pi v = TauExp Bool) then
	env.boolexp v s
      else
	raise (Failure ((string_of_expression e) ^ " is not a Boolean expression."))
  | _ -> raise (Failure ((string_of_expression e) ^ " is not a Boolean expression."))
;;

(* ---------------------------------------------------------------------- *)
(* meaning of commands and declarations                                   *)
(* ---------------------------------------------------------------------- *)

let rec meaning_of_command c pi env s =
  match c with 
    Assign (al,e) -> update (meaning_of_loc al env, meaning_of_int_expression e pi env s, s)
  | Seq (c1,c2) -> meaning_of_command c2 pi env (meaning_of_command c1 pi env s)
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
  | Call(p,e) -> 
      let t1 = 
        (match (type_of_id pi p) with
           Arrow(t1,Comm) -> t1
         | _ -> raise (TypeError  e))
      in
        (match t1 with
           TauExp Int -> 
             env.proc_int p (fun s -> meaning_of_int_expression e pi env s) s
         | TauExp Bool -> 
             env.proc_bool p (fun s -> meaning_of_bool_expression e pi env s) s
         | _ -> 
             raise (TypeError  e))
;;

let rec meaning_of_declaration d pi env s  = 
  match d with
      Var id -> 
	let (l,s') = allocate s in	  
	  (update_intloc_env (id, l) env0, s')
    | Fun (id, body) -> 
	(match (expression_type body pi) with
	     Int -> (update_intexp_env (id, fun s' -> meaning_of_int_expression body pi env s') env0, s)
	   | Bool -> (update_boolexp_env (id, fun s' -> meaning_of_bool_expression body pi env s') env0,s)
	)
    | Semi (d1,d2) -> 
	let pi1 = declaration_type d1 pi in
	let (e1,s1) = meaning_of_declaration d1 pi env s in
	let (e2,s2) = meaning_of_declaration d2 (union_ta pi pi1) (union_env env e1) s1 in
          (union_env e1 e2, s2)
    | Comma (d1,d2) -> 
	let (e1,s1) = meaning_of_declaration d1 pi env s in
        let (e2,s2) = meaning_of_declaration d2 pi env s1 in
	  (union_env e1 e2, s2)
    | Proc (i1, i2, t, c) ->
	match t with
	    (TauExp Int) -> 
              let type_assignment = bar_union_ta pi (TA [(i2,t)]) in 
              let p f s' = meaning_of_command c type_assignment (union_env  env (update_intexp_env (i2,f) env0)) s' in
              let ienv = update_proc_int_env (i1,p) env0 in
              (ienv,s) 
	  | (TauExp Bool) -> 
              let type_assignment = bar_union_ta pi (TA [(i2,t)]) in 
              let p f s' = meaning_of_command c type_assignment (union_env  env (update_boolexp_env (i2,f) env0)) s' in
              let benv = update_proc_bool_env (i1,p) env0 in
              (benv,s) 
	  | _ -> raise (Failure "meaning_of_declaration: unexpected argument type!")
;;

(* ---------------------------------------------------------------------- *)
(* meaning of programs                                                    *)
(* ---------------------------------------------------------------------- *)

let meaning_of_program (Prog (d,c)) s =
 let pi = declaration_type d pi0 in
 let (env, s1) = meaning_of_declaration d pi0 env0 s in
    meaning_of_command c pi env s1
;;

