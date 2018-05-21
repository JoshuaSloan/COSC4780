(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2011
*)

(* Schmidt, Structure of Typed Programming Languages --
   This code parses the language from chapter 1 in Schmidt's book which has been 
   extended to support a side-effecting expression L<-E 
*)

#use "./parser_base.ml" ;;


(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)
(*   L ::= locj   where j is a natural number e.g. loc2 or loc7           *)
(* ---------------------------------------------------------------------- *)

let locationP = bind (prefix "loc") (fun _ -> bind natural (fun i -> return (Loc i)));;

(* some tests *)

(* parse (bind identifier (fun i -> return (I i))) "x";; *)
(* parse locationP "x";; *)


(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)
(*   E ::= T = E | T + E | T                                              *)
(*   T ::= N | @L | ~E | L <- E | (E)                                     *)
(* ---------------------------------------------------------------------- *)

let symEq = symbol "=";;
let symPlus = symbol "+";;
let symAt = symbol "@";;
let symNot = symbol "~";;
let symAss = symbol "<-";;

let numP = bind integer (fun i -> return (Num i));;

let derefP = bind symAt (fun _ -> bind locationP  (fun k -> return (Deref k)));;


let notP p = bind symNot (fun _ -> bind (p ()) (fun e -> return (Not e)));;

let eqP p x  = bind symEq (fun _ -> bind (p ()) (fun y -> return (Eq(x, y))));;
let plusP p x  = bind symPlus (fun _ -> bind (p ()) (fun y -> return (Plus(x, y))));;

let assignEP p = 
  bind locationP 
    (fun loc -> bind symAss 
       (fun _ -> bind (p ())
	  (fun e -> return (AssignE(loc,e)))));;


let rec expressionP =
  let rec p _ = choices  [ bind (t ()) (fun x -> f x); t ()]
  and f x = choices [eqP t x; plusP t x]
  and t _ = choices [numP; derefP ; notP p; assignEP p;  wrapped p ] 
  in
    p ()
;;

(* some tests *)

(* parse expressionP  "1+2";;  *)
(* parse expressionP "2 + @loc9";; *)
(* parse expressionP "2 = 1";; *)
(* parse expressionP "(2= @loc5) + 7";; *)
(* parse expressionP "~(0=0)";; *)
(* parse expressionP "(loc1<-5)" ;;*)
(* parse expressionP "(loc1<-5)+2" ;;*)
(* parse expressionP "(2= (loc1 <- @loc5)) + 7";; *)


(* ---------------------------------------------------------------------- *)
(* commands                                                               *)
(* ---------------------------------------------------------------------- *)
(*   C ::=  F ; C | F                                                     *)
(*   F ::= L := E | if E then C else C fi                                 *)
(*          | while E do C od | skip | (C)                                *)
(* ---------------------------------------------------------------------- *)

let symSemi = symbol ";";;
let symAssign = symbol ":=";;
let symIf = symbol "if";;
let symThen  = symbol  "then";;
let symElse  = symbol "else";;
let symFi  = symbol "fi";;
let symWhile = symbol "while" ;;
let symDo = symbol  "do";;
let symOd = symbol "od" ;;
let symCall = symbol "call" ;;

let assignP = 
  bind locationP 
    (fun loc -> bind symAssign 
       (fun _ -> bind expressionP 
	  (fun e -> return (Assign(loc,e)))));;

let ifthenelseP c = 
  bind' symIf 
    (bind expressionP 
       (fun e -> bind' symThen 
	  (bind  (c ()) 
	     (fun c1 -> bind' symElse 
		(bind (c ()) 
		   (fun c2 -> bind' symFi (return (Ite(e,c1,c2)))))))))
;;

let whileP cp = 
  bind' symWhile
  (bind expressionP 
     (fun e -> bind' symDo 
	(bind (cp ()) (fun c -> bind' symOd  (return (While(e,c)))))))
;;

let skipP = bind' (symbol "skip") (return Skip);;


let seqP cp1 cp2 = 
  bind (cp1 ()) (fun e1 -> bind' symSemi (bind (cp2 ()) (fun e2 -> return (Seq(e1,e2)))));;

let commandP = 
  let rec parse_c _ =  
    choice (seqP parse_f parse_c) (parse_f ())
  and parse_f _  = 
    choices [assignP; ifthenelseP parse_c; whileP parse_c; skipP; wrapped parse_c ] 
  in 
    parse_c ()
;;


(* some tests  *)


(* parse commandP "loc1 := 10";; *)
(* parse commandP "skip; skip";; *)
(* parse commandP "skip; skip; skip";; *)
(* parse commandP "(skip;skip);skip";; *)
(* parse commandP "if (1 = @loc1) then skip else loc1 := @loc1 + 1 fi";; *)
(* parse commandP "while ~(@loc1 = 0) do loc1:= @ loc1 + 1 od" ;; *)
(* parse commandP "loc7 := 10 ;if (1 = @loc1) then skip else loc1 := @loc1 + 1 fi";; *)
(* parse commandP "if @loc7 = 0 then loc7 := 100 else loc7 := (-100) fi";; *)


