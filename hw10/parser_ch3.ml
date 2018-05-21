(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2010
*)

(* Schmidt, Structure of Typed Programming Languages --
   This code parses the language from chapter 3 in Schmidt's book.
   with unparameterized functions and procedures parameterized by expressions.

 *)


(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)
(*   L ::= I | locj   where j is a natural number e.g. loc2 or loc7           *)
(* ---------------------------------------------------------------------- *)
let symI = symbol "I";;

let locationP = bind identifier (fun i -> return (I i));;

(* some tests *)

(* parse (bind identifier (fun i -> return (I i))) "x";; *)
(* parse locationP "x";; *)


(* ---------------------------------------------------------------------- *)
(* expression types                                                       *)
(* ---------------------------------------------------------------------- *)
(*   T ::= Int | Bool                                                     *)
(* ---------------------------------------------------------------------- *)

let expression_typesP = 
    choices [bind' (symbol "Int") (return (Int)); 
             bind' (symbol "Bool") (return (Bool))] 
;;

(* ---------------------------------------------------------------------- *)
(* types                                                            *)
(* ---------------------------------------------------------------------- *)
(*   TY ::= TY11 -> TY | TY1                                              *)
(*   TY1 ::= intloc | comm | Texp | (TY)                                        *)
(* ---------------------------------------------------------------------- *)

let symArrow = symbol "->";;

let intlocP = bind (symbol "intloc") (fun _ -> return Intloc);;
let commP  = bind (symbol "comm") (fun _ -> return Comm);;

let arrowP p x = bind symArrow (fun _ -> bind (p ()) (fun y -> return (Arrow (x,y))));;
let texpP = bind expression_typesP (fun t -> bind (symbol "exp") (fun _ -> return (TauExp t)));;

let typesP = 
  let rec ty _ = choices [bind (ty1 ())(fun x -> arrowP ty x); ty1 ()] 
  and ty1 _ = choices [intlocP; commP ; texpP;  wrapped ty]
  in
    ty ()
;;

(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)
(*   E ::= E1 = E | E1 + E | E1                                              *)
(*   E1 ::= N | @L | I |~E | (E)                                           *)
(* ---------------------------------------------------------------------- *)

let symEq = symbol "=";;
let symPlus = symbol "+";;
let symAt = symbol "@";;
let symNot = symbol "~";;

let numP = bind integer (fun i -> return (Num i));;
let derefP = bind symAt (fun _ -> bind locationP  (fun k -> return (Deref k)));;
let funcallP = bind identifier (fun i -> return (Id i))  ;;

let notP p = bind symNot (fun _ -> bind (p ()) (fun e -> return (Not e)));;

let eqP p x  = bind symEq (fun _ -> bind (p ()) (fun y -> return (Eq(x, y))));;
let plusP p x  = bind symPlus (fun _ -> bind (p ()) (fun y -> return (Plus(x, y))));;

let expressionP =
  let rec p _ = choices  [ bind (t ()) (fun x -> f x); t ()]
  and f x = choices [eqP t x; plusP t x]
  and t _ = choices [numP; derefP ; notP p; funcallP; wrapped p ] 
  in
    p ()
;;

(* some tests *)

(* parse expressionP  "1+2";; *)
(* parse expressionP "f    + 2";; *)
(* parse expressionP "2 + f";; *)
(* parse expressionP "2 = f";; *)
(* parse expressionP "(2= f) + g";; *)
(* parse expressionP "(2= @x) + g";; *)
(* parse expressionP "@x";; *)
(* parse expressionP "~(0=0)";; *)




(* ---------------------------------------------------------------------- *)
(* commands                                                               *)
(* ---------------------------------------------------------------------- *)
(*   C ::=  F ; C | F                                                     *)
(*   F ::= L := E | if E then C else C fi                                 *)
(*          | while E do C od | skip | Call I(E) | (C)                       *)
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
let symLParen = symbol "(";;
let symRParen = symbol ")";;
let symColon = symbol ":";;
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

let callP = bind' (symbol "call") 
           (bind identifier (fun id ->
           (bind' (symbol "(")
           (bind expressionP (fun e -> 
           (bind' (symbol ")") 
             (return (Call (id, e)))))))))
;;


let seqP cp1 cp2 = 
  bind (cp1 ()) (fun e1 -> bind' symSemi (bind (cp2 ()) (fun e2 -> return (Seq(e1,e2)))));;

let commandP = 
  let rec parse_c _ =  
    choice (seqP parse_f parse_c) (parse_f ())
  and parse_f _  = 
    choices [assignP; ifthenelseP parse_c; whileP parse_c; skipP; callP; wrapped parse_c ] 
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
(* parse commandP "x := 10 ;if (1 = @loc1) then skip else loc1 := @loc1 + 1 fi";; *)
(* parse commandP "if x = 0 then ans := 100 else ans := (-100) fi";; *)



(* ---------------------------------------------------------------------- *)
(* declarations                                                           *)
(* ---------------------------------------------------------------------- *)
(*  D  ::= D';D | D',D | D'                                               *)
(*  D' ::= var I | fun I = E | proc I1 (I2 : T) = C | (D)                           *)
(*                                                                        *)
(* ---------------------------------------------------------------------- *)

let symComma = symbol "," ;;
let symVar = symbol "var" ;;
let symFun = symbol "fun" ;;
let symProc = symbol "proc" ;;

let semiP dp1 dp2 =  bind (dp1 ()) (fun d1 -> (bind' symSemi (bind (dp2 ()) (fun d2 -> return (Semi(d1,d2))))));;
let commaP dp1 dp2  = bind (dp1 ()) (fun d1 -> (bind' symComma (bind (dp2 ()) (fun d2 -> return (Comma(d1,d2))))));;

let varP = bind' symVar (bind identifier (fun i -> return (Var i)));;
let funP = bind' symFun (bind identifier (fun i -> bind' symEq (bind expressionP (fun e -> return (Fun(i,e))))));;
let procP = bind' symProc 
           (bind identifier (fun i1 -> 
           (bind' (symbol "(")
           (bind identifier (fun i2 -> 
           (bind' (symbol ":") 
           (bind typesP (fun t -> 
           (bind' (symbol ")" )
           (bind' symEq 
           (bind commandP (fun c -> 
              return (Proc(i1,i2,t,c))))))))))))))
;;

let declarationP = 
  let rec declP _ = 
    choices [semiP declP' declP; commaP declP' declP; declP' ()]
  and declP' _ = 
    choices [varP; funP; procP; wrapped declP ]
  in 
    declP ()
;;
   

(* some tests *)
   

(* parse declarationP  "var x; var y";; *)
(* parse declarationP "fun f = @x ";; *)
(* parse declarationP "var x ; fun f = @x ";; *)
(* parse declarationP "var x , fun f = @x ";; *)
(* parse declarationP "fun a = @loc1 + 1; fun b = a + a";;  *)
(* parse declarationP "proc P(I:Int) =  x := @x + I";;   *)
(* parse declarationP "proc Q(B:Bool) =  if B then x := @x + 1 else skip fi";;  *)



(* ---------------------------------------------------------------------- *)
(* program                                                           *)
(* ---------------------------------------------------------------------- *)
(*  P  ::= D in C                                                         *)
(* ---------------------------------------------------------------------- *)

let symIn = symbol "in";;

let programP = bind declarationP (fun d -> bind' symIn (bind commandP (fun c -> return (Prog(d,c)))));;


(* some tests *)


(* parse programP "var x; var y; var temp ; proc P = temp := @x; y:= @x; y:= temp in x := (-1);y:=@x + 2; call P";;  *)
(* parse programP  "var x; fun F = @x+2 in x := F ";; *)
(* parse programP "var x; var y; var temp ; proc swap = temp := @x; x:= @y; y:= @temp in x := (-2);y:=@x + 4; call swap";;  *)
(* parse programP   "var x; var y; var ans  in ans := 0; x:= 0; y := 12; while ~(@x = 0) do ans := @ans + @y; x := @x + (-1) od ";; *)
(* parse programP   "var x; var y; var ans  in ans := 0; x:= 12; y := 0; while ~(@x = 0) do ans := @ans + @y; x := @x + (-1) od ";; *)
(* parse programP   "var x; var y; var ans  in ans := 0; x:= 1; y := 12; while ~(@x = 0) do ans := @ans + @y; x := @x + (-1) od ";; *)
(* parse programP   "var x; var y; var ans  in ans := 0; x:= 12; y := 1; while ~(@x = 0) do ans := @ans + @y; x := @x + (-1) od ";; *)
(* parse programP   "var x; var y; var ans  in ans := 0; x:= 12; y := 12; while ~(@x = 0) do ans := @ans + @y; x := @x + (-1) od ";; *)
