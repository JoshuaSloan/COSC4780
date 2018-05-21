(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2012
*)

(* test code for HW16e *)


(* ====================================================================== *)
(* Load the Parser                                                        *)
(* ====================================================================== *)

#use "hw10j.ml";;
#use "parser_base.ml" ;;
#use "parser_ch3.ml" ;;



(* this function provides a template for testing meaning_of_declaration *)


let test_mod d= 
  let d' = analyze declarationP d in
  let pi1 = declaration_type d' pi0 in
  let (env,s') = meaning_of_declaration  d' pi0 env0 s0 in
    print_ta pi1;
    print_env pi1 env s';
    env
;;

(* this function provides a template for testing meaning_of_program *)

let test_mop p =
  let p' = analyze programP p in
  let s1 = meaning_of_program p' s0 in
    print_store s1
;;


test_mod "var x";;
test_mod "var x , var y";;
test_mod "var y , var x";;
test_mod "var x; var y; proc double (x :Intexp) = (y := x + x)" ;;
test_mod "var x; var y; proc add(x :Intexp) = y := x + @y ; proc double(z: Intexp) = x := (z + z)" ;;
test_mop "var x; var y; proc add(x :Intexp) = y := x + @y ; proc double(z: Intexp) = x := (z + z) in x := 2; y := 4; call add(@x); call double(@y)";;
test_mod "var A;  proc M(x :Intexp) = (A := x; A:= x)";;
test_mop "var A;  proc M(x :Intexp) = (A := x; A:= x) in A := 2; call M(@A+1)";;
test_mop "var A; proc P(M : Boolexp) = (if M then A:= 1 else A:= 2 fi) in call P(@A=@A)";;
test_mop "var A; proc P(M : Boolexp) = (if M then A:= 1 else A:= 2 fi) in call P(@A=10)";;

