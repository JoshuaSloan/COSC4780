 (* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* test data for HW 8 *)

#use "hw8i.ml";;

let test_decl d pi = 
   print_string (string_of_declaration d);
   print_string  "  -->  ";
   print_ta (declaration_type d pi);
   print_string "\n"
;;



let test_prog p = 
   print_string (string_of_program p);
   print_string (if (well_typed_program p) then "is well-typed." else "is not well-typed.");
   print_string "\n"
;;



test_decl (Fun("X",Eq (Num 0, Num 0))) pi0;;
test_decl (Fun("X",Not (Eq (Num 0, Num 0)))) pi0;;
test_decl (Fun("X",Num 0)) pi0;;
test_decl (Comma(Fun("X",Num 0),Fun ("Y", Id "Z"))) pi0;;
test_decl (Comma(Fun("X",Num 0),Fun ("Y", Id "X"))) pi0;;
test_decl (Comma(Fun("X",Num 0),Fun ("Y", Eq(Num 0, Num 0 )))) pi0;;
test_decl (Semi(Fun("X",Num 0),Fun ("Y", Id "X"))) pi0;;

test_prog (Prog(Fun ("False",Eq (Num 0, Num 1)), Ite (Id "False", Assign (Loc 1, Num 1), Assign (Loc 2, Num 2))));;
test_prog (Prog(Fun ("False",Eq (Num 0, Num 1)), Ite (Id "True", Assign (Loc 1, Num 1), Assign (Loc 2, Num 2))));;
test_prog (Prog(Comma(Fun ("True",Eq (Num 0, Num 0)),Fun ("X", Num 42)), Ite (Id "True", Assign (Loc 1, Id "X"), Assign (Loc 2, Num 2))));;
test_prog (Prog(Comma(Fun ("True",Eq (Num 0, Num 0)),Fun ("X", Num 42)), Ite (Id "True", Assign (Loc 1, Id "True"), Assign (Loc 2, Num 2))));;
