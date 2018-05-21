(* Josh Sloan *)
(* HW2 *)
(* I worked with CJ and Jacob on this assignment *)
(* Some code to base HW 2 on *)

type lambda = Var of string | Ap of lambda * lambda | Lambda of string * lambda ;;

let rec pretty t = 
  match t with 
      Var x -> x
    | Ap (m,n) -> "(" ^ pretty m ^ " " ^ pretty n ^ ")"
    | Lambda (x,m) -> "(lambda " ^ x ^ ". " ^ pretty m ^ ")"
;; 

let remove_all x xs = List.filter (fun y -> not (y = x)) xs;;

let rec fv t = 
  match t with 
      Var x -> [x]
    | Ap (m,n) -> fv m @ (fv n)
    | Lambda (x,m) -> remove_all x (fv m)
;; 


(* fresh - returns a fresh variable - w.r.t. the list m *)
let fresh x m = 
  let rec fresh' x k =
    let x_k = x ^ string_of_int k in
      if List.mem x_k m then
	fresh' x (k+1) 
      else
	x_k
  in
    if List.mem x m then
      fresh' x 0 
    else 
      x
;;

  
let your_code_goes_here ()= failwith "Your code goes here!" ;;

let rec subst (x,n) m = 
  match m with
      Var y -> if y = x then n else m
    | Ap (m1,m2) -> Ap (subst (x,n) m1, subst (x,n) m2)
    | Lambda (y,m1) ->  if x=y then Lambda(y,m1)
    else if not (List.mem y (fv n)) then Lambda (y, (subst(x,n) m1))
    else let z = fresh "z" ((fv n)@(fv m1)@[x]@[y]) in
    subst(x,n) (Lambda(z,subst(y,Var z) m1))
;;




(* some tests *)

let test (x, t1) t2= 
  let out = pretty t2 ^ "[" ^ x ^ ":= " ^ (pretty t1) ^ "]  =   " ^ pretty (subst (x,t1) t2) ^ "\n\n" in
   Printf.printf "%s" out
;; 


test ("x", Var "y") (Var "z");;
test ("x",  Var "y") (Var "x");;
test ("x", Var "z") (Ap (Var "x", Var "y"));;
test ("x",  Lambda ("x", Var "y")) (Var "x");;
test ("y",  Var "z") (Lambda ("x", Var "y"));;
test ("x",  Var "y") (Lambda ("y", Var "x"));;
test ("x",  Var "y") (Lambda ("y", Ap(Var "y", Var "x")));;
 

