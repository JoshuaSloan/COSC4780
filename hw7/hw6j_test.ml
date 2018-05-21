(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* test code for hw6 *)

#use "hw6j.ml";;
#use "parser_base.ml";;
#use "parser_ch1.ml";;



let test f  p = 
    f (analyze commandP p)
;;

let test_exp_refs e = exp_refs (analyze expressionP e);;


test_exp_refs "@loc2 = 1";;
test_exp_refs "(1 = 1) = ~(2 = 2)";;
test_exp_refs "(1 = @loc1) = ~(2 = @loc2)";;
test_exp_refs "@loc1 = @loc2";;
test_exp_refs "@loc3 = @loc1";;
test_exp_refs "@loc3 + @loc1";;
test_exp_refs "@loc3 + 1";;

test active_locs "loc1 := @loc1";;
test active_locs "loc1 := @loc2";;
test active_locs "loc1 := (-2); while ~(@loc1 = 1) do loc1:= @loc2 + 1 od";;
test active_locs "loc1 := (-2); while ~(@loc1 = 1) do loc1:= @loc2 + 1; loc2 := @loc1 od";;
test active_locs "if @loc1 = @loc2 then loc3 := @loc2 + 1 else loc4 := @loc 1 + 1 fi";;

test loops_analysis "loc1 := @loc1";;
test loops_analysis "while (@loc1 = 0) do loc1 := @loc1 + 1 od";;
test loops_analysis "while (@loc2 = 0) do loc1 := @loc1 + 1 od";;
test loops_analysis "while (@loc2 = 0) do loc1 := @loc1 + 1 od; while (@loc2 = 1) do loc2 := 1 od";;
test loops_analysis "while (@loc2 = 0) do loc1 := @loc1 + 1 od; while (@loc2 = 1) do loc1 := 1 od";;
test loops_analysis "if (0=0) then while (@loc1 = 0) do loc1 := @loc1 + 1 od else skip fi";;
test loops_analysis "if (0=0) then skip else while (@loc1 = 0) do loc1 := @loc1 + 1 od  fi";;

test loops_analysis "if (0=0) then while (@loc2 = 0) do loc1 := @loc1 + 1 od else skip fi";;
test loops_analysis "if (0=0) then skip else while (@loc2 = 0) do loc1 := @loc1 + 1 od fi" ;;


