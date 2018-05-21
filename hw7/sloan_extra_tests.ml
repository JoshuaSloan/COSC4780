#use "hw6j.ml";;
#use "parser_base.ml";;
#use "parser_ch1.ml";;

let test f  p = 
    f (analyze commandP p)
;;

let test_exp_refs e = exp_refs (analyze expressionP e);;

test loops_analysis "if (1=0) then skip else if (0=0) then while (@loc2 = 0) do loc1 := @loc1 + 1 od else while (@loc1 = 0) do loc1 := @loc1 + 1 od fi fi";;
test loops_analysis "if (0=0) then while (@loc2 = 0) do loc1 := @loc1 + 1 od else while (@loc1 = 0) do loc1 := @loc1 + 1 od fi";;
test loops_analysis "if (0=0) then while (@loc1 = 0) do loc1 := @loc1 + 1 od else while (@loc2 = 0) do loc1 := @loc1 + 1 od fi" ;;
test loops_analysis "if (1=0) then skip else if (1=0) then skip else if (1=1) then skip else while (@loc1 = 0) do loc1 := @loc1 + 1 od fi fi fi";;