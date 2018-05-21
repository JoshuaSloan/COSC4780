(* Josh Sloan *)
(* HW4 *)
(* debruijn.ml *)

type lambda = Var of string | Ap of lambda * lambda | Abs of string * lambda ;;

type debruijn  = DBIndex of int | DBVar of string | DBAp of debruijn * debruijn | DBAbs of debruijn ;;

let rec pretty_debruijn t = 
  match t with 
      DBIndex k -> string_of_int k
    | DBVar x -> x
    | DBAp (m,n) -> "(" ^ pretty_debruijn m ^ " " ^ pretty_debruijn n ^ ")"
    | DBAbs m -> "(lambda " ^ pretty_debruijn m ^ ")"
;;

let update f x = 
  fun y -> 
    if y = x then
      DBIndex 1
    else
      match (f y) with
	  (DBIndex k) -> DBIndex (k + 1)
	| (DBVar z) -> DBVar z
	| _ -> failwith "dbUpdate: how did this happen?"
;;

let debruijnize t = 
  let f x = DBVar x in
  let rec doit f t =
          match t with
            Var x1 -> f x1
          | Ap (m1,n1) -> DBAp (doit f m1,doit f n1)
          | Abs (x1,m1) -> let f' = update f x1 in
                            DBAbs (doit f' m1)
  in
     doit f t
;;

(* test for debruijnize functionality *)
let test t = pretty_debruijn (debruijnize t);;

(* compare two strings returned from pretty_debruijn representation (i.e. test function) for equivalence.
If the strings are equal, then they are alpha equivalent.
NOTE: Return value given as a string for printing! *)
let alphaeq t1 t2 =
   let t1 = test t1 in
   let t2 = test t2 in
   if t1 = t2 then let str = string_of_bool true in
      str
   else let str = string_of_bool false in
      str
;;

(* test for alpha equivalence *)
let test2 j k  = alphaeq j k;;

(* print input followed by 2 new lines *)
let print input = let out = input ^ "\n\n" in
   Printf.printf "%s" out
;;

(* print input followed by 1 new line *)
let prove input = let out = input ^ "\n" in
   Printf.printf "%s" out
;; 

(* test cases *)

test (Abs ("x",Var "x"));;
test (Abs ("y",Var "y"));;
test (Abs ("y",Var "x"));;
test (Abs ("x", Ap(Abs ("x",Var "x"), Abs ("y", Ap (Var "x", Var "y")))));;

prove "Abs (x,Var x)";;
print (test (Abs ("x",Var "x")));;

prove "Abs (y,Var y)";;
print (test (Abs ("y",Var "y")));;

prove "Abs (y,Var x)";;
print (test (Abs ("y",Var "x")));;

prove "Abs (x, Ap(Abs (x,Var x), Abs (y, Ap (Var x, Var y))))";;
print (test (Abs ("x", Ap(Abs ("x",Var "x"), Abs ("y", Ap (Var "x", Var "y"))))));;

prove "Abs (x,Var x) alphaEQ to Abs (y,Var y)?";;
print (test2 (Abs ("x",Var "x")) (Abs ("y",Var "y")));;

prove "Abs (x, Ap(Abs (x,Var x), Abs (y, Ap (Var x, Var y)))) alphaEQ to Abs (y,Var x)?";;
print (test2 (Abs ("x", Ap(Abs ("x",Var "x"), Abs ("y", Ap (Var "x", Var "y"))))) (Abs ("y",Var "x")));;
