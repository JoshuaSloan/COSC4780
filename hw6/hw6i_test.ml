(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* test code for hw6 *)


#use "hw6i.ml";;
#use "parser_base.ml" ;;
#use "parser_ch1a.ml" ;;


let test_exp e s = 
  let e = analyze expressionP e in
    match (expression_type e) with
	Boolexp -> 
	  let (b,s1) = meaning_of_bool_expression e s in
            print_string "initial store:\n";
            print_store s;
            print_string ((string_of_expression e) ^ " evaluates to " ^ string_of_bool b ^ "\n");
            print_string "final store:\n";
	    print_store s1 
      | Intexp ->
	  let (i,s1) = meaning_of_int_expression e s in
	    print_string "initial store:\n";
	    print_store s;
	    print_string ((string_of_expression e) ^ " evaluates to " ^ string_of_int i ^ "\n");
	    print_string "final store:\n";
	    print_store s1 
;;


let test_comm cs s = 
  let c = analyze commandP cs in
  let s1 = meaning_of_command c s in
    print_string (cs ^ "  maps:\n");
    print_store s;
    print_string "to :\n";
    print_store s1 
;;

(*--------------------------------------------------------------------------------*)
(* expected output is the result of evaluating code after this point.             *)
(*--------------------------------------------------------------------------------*)

let s0 = Store (4,fun (Loc i) -> 0 );;
let si = Store (4,fun (Loc i) -> i );;

test_exp "loc1 <- (10 + @loc1)" s0;;
test_exp "(loc1 <- 2) + (loc1 <- 3)"  s0;;
test_exp "(loc1 <- 3) + (loc1 <- 2)"  s0;;

test_exp "@loc2 = loc2 <- 3" si;;
test_exp "(loc2 <- 3) = @loc2" si;;
test_exp "(loc2 <- (loc3 <- @loc1))" si;;

test_comm "loc1 := (loc1 <- 10) + @loc1" s0;;
test_comm "loc1 := @loc1 + (loc1 <- 10)" s0;;
test_comm "while (~ (10 = (loc1 <- (@loc1+1)))) do loc2 := @loc1 + @loc1 od" s0;;
test_comm "if (@loc3 = (loc1<- 3)) then loc4 := @loc1 + (loc1 <- (-1)) else skip fi " si;;
test_comm "if (@loc3 = (loc1<- 3)) then loc4 := @loc1 + (loc1 <- 1) else skip fi " si;;

(* swap - loc1 and loc2 -- ruining loc 3 *)
test_comm "loc3 := @loc1; loc1 := @loc2; loc2 := @loc3" si;;

test_comm "if ((loc3 <- @loc1) = @loc3) then loc1 := @loc2; loc2 := @loc3 else skip fi" si;;
