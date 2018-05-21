(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* test code for HW 9 *)
(* ---------------------------------------------------------------------- *)
(* testing                                                                *)
(* ---------------------------------------------------------------------- *)
#use "./hw9j.ml";;
#use "./record_parser.ml";;

(* If you need to see what the unforgiving parser produces -- use the view function *)

let view p = analyze programP p;;

let testp p = 
  let p' = analyze programP p in
    if well_typed_program p' then 
      let d = match p' with Prog(d,c) -> d  in
      let pi = declaration_type d pi0 in
      let (env,s) = meaning_of_declaration d pi env0 s0 in
	print_string ("\n prog = " ^ p);
	print_string "\n pi = ";
	print_ta pi;
	print_string "\n env = ";      
	print_env pi env s;
	print_string "\n store = ";      
	print_store (meaning_of_program p' s0)
    else
      raise (Failure (p ^ " is not well typed."))
;;


let test_decl d = 
  let d' = analyze declarationP d in
    if well_typed_declaration d' pi0 then
      let pi = declaration_type d' pi0 in
      let (e,s) = meaning_of_declaration d' pi env0 s0 in
	print_string("\n decl = " ^ (string_of_declaration d'));
	print_string "\n pi = ";
	print_ta pi;
	print_string "\n env = ";
	print_env pi e s
    else
      raise (Failure (d ^ " is not well typed."))
;;



test_decl "class M = record {class N = newint}; var X :M; var i : (X.N) ";;
test_decl "class P = record { class Q = newint}";;
test_decl "class P = record { var x : newint }";;
test_decl "class M = record {class N = record { var y : newint}; var x: N}; var z : M";;


testp "var A : newint; class R = record {var B : newint; proc P = A := @A + @B}; var r1 : R  in r1.B := 1; A := 2; call r1.P" ;;

 testp "var I:newint in I:= 1";;
testp "var I:newint in I:=@I+1; I:= @I + @I";;
testp "class M = newint; var i:M in while ~(@i = 0) do i := @i +(-1) od" ;;
testp "class M = newint; var i:M in i:= 10; while ~(@i = 0) do i := @i +(-1) od" ;;
testp "class M =  record {var x:newint}; var i:M in i.x := 10";;
testp "class M =  record {var x:newint}; var i:M; var j:newint ; var ans : newint in i.x := 10; j:= 100; while ~(@i.x = 0) do i.x := @i.x +(-1); ans := @ans + @j   od" ;;
testp "class M = record {var x:newint; var y:newint}; var i:M in i.x := 10; i.y := 20; i.x := @i.y + @i.x" ;;
testp "class M = record {var x:newint; var y:newint}; var i:M in i.x := 10 ; i.y := 11; if ~(@i.y = 10) then i.x := @i.x + @i.y else i.y := @i.x + 5 fi";;
testp "class M = record {var x:newint; var y:newint}; var i:M in i.x := 10 ; i.y := @i.x; if ~(@i.y = 10) then i.x := @i.x + @i.y else i.y := @i.x + 5 fi";;
testp "class M = record {var x:newint; var y:newint}; var i:M in i.x := 10; i.y := 0; while ~(@i.x = 0) do i.y := @i.y + @i.x; i.x := @i.x + (-1) od" ;;
testp "class M =  record {var x:newint}; var i : M; var j :M in i.x := @i.x + (-1); j.x := 21";;
testp "class M =  record {var x:  record {var y:newint}}; var i : M in i.x.y := 10 ";;
testp "class M =  record {var x:  record {var y:newint}}; var i : M in i.x.y := 10 ; i.x.y := @i.x.y + @i.x.y";;
testp "class N =  record {var y:newint}; class M =  record {var x: N}; var i : M in i.x.y := 10 ; i.x.y := @i.x.y + @i.x.y";;
testp "class M =  record {var x:  record {var y:newint}}; var i : M in i.x.y := 10 ; i.x.y := @i.x.y + @i.x.y";;
testp "class M =  record {class N =  record {var y:newint};var x: N}; var i : M in i.x.y := 10 ; i.x.y := @i.x.y + @i.x.y";;
testp "class M =  record {class N =  record {var y:newint};var x: N; var z: N}; var i : M in i.x.y := 10 ; i.z.y:= 100; i.x.y := @i.x.y + @i.z.y";;
testp "const True = (0=0); var x: newint in if True then x := 1 else x := 0 fi";;
testp "const False = (0=1); var x: newint in if False then x := 1 else x := (-1) fi";;
