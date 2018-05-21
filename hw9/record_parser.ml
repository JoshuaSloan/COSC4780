(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2010
*)

(* Schmidt, Structure of Typed Programming Languages --
   This code parses the language from chapter 2 in Schmidt's book. *)

#use "parser_base.ml" ;;


(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)
(*   L ::= locj   where j is a natural number e.g. loc2 or loc7           *)
(* ---------------------------------------------------------------------- *)

let locationP =  bind (prefix "loc") (fun _ -> bind natural (fun i -> return (Loc i)));;

(* some tests *)

(* parse (bind identifier (fun i -> return (I i))) "x";; *)
(* parse locationP "x";; *)


(*----------------------------------------------------------------------*)
(* Identifiers                                                          *)
(*----------------------------------------------------------------------*)
(*                                                                      *)
(* X ::= T.X | T                                                      *)
(* T ::= I | (X)                                                                     *)
(*----------------------------------------------------------------------*)

let symPeriod = string ".";;

let build_XId m =
  match m with 
      [] -> raise (Failure "build_XId: empty list.")
    |x::t -> List.fold_left (fun id s -> XId (id,s)) (Id x) t

let rec identifierP =  
  let idP  = 
    bind' whiteSpace
      (bind identifier 
	 (fun x -> (bind (listP (bind symPeriod (fun _ -> identifier)))
		      (fun xs -> return (x::xs)))))
  in 
    bind (choices[wrapped (fun _ -> idP); idP]) (fun xs -> return (build_XId xs))  
;;


(* some tests *)
(* parse identifierP "X.i";; *)
(* parse identifierP "i";; *)
(* parse identifierP "X.Y.i";; *)
(* parse identifierP "x.y.z.w  x.x.x";;  *)
parse identifierP "x . y.z.w  x.x.x";; 
(* parse identifierP "(x.y)";; *)

(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)
(*   E ::= T = E | T + E | T                                              *)
(*   T ::= N | @L | I |~E | (E)                                           *)
(* ---------------------------------------------------------------------- *)

let symEq = symbol "=";;
let symPlus = symbol "+";;
let symAt = symbol "@";;
let symNot = symbol "~";;

let numP = bind integer (fun i -> return (Num i));;
let derefP = bind symAt (fun _ -> bind identifierP  (fun k -> return (Deref k)));;
let funcallP = bind identifierP (fun i -> return (Funcall i))  ;;

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
(*          | while E do C od | skip | Call I | (C)                       *)
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
  bind identifierP 
    (fun id -> bind symAssign 
       (fun _ -> bind expressionP 
	  (fun e -> return (Assign(id,e)))));;

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

let callP = bind' (symbol "call") (bind identifierP (fun id -> return (Call id)));;


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

parse commandP "x := 10";; 
parse commandP "i.x.y := @i.x + 10";; 
(* parse commandP "skip; skip";; *)
(* parse commandP "skip; skip; skip";; *)
(* parse commandP "(skip;skip);skip";; *)
(* parse commandP "if (1 = @loc1) then skip else loc1 := @loc1 + 1 fi";; *)
(*  parse commandP "while ~(@loc1 = 0) do loc1:= @ loc1 + 1 od" ;; *)
(* parse commandP "x := 10 ;if (1 = @loc1) then skip else loc1 := @loc1 + 1 fi";; *)
(* parse commandP "if x = 0 then ans := 100 else ans := (-100) fi";; *)



(* ----------------------------------------------------------------------    *)
(* declarations and type structures                                          *)
(* ----------------------------------------------------------------------    *)
(*  D  ::= D';D | D',D | D'                                                  *)
(*  D' ::= var I | fun I = E | proc I = C | class I = T | Const I = E  | (D) *)
(*  T  ::= newint | record {D} | identifier                                  *)
(* ----------------------------------------------------------------------    *)

let symComma = symbol "," ;;
let symVar = symbol "var" ;;
let symFun = symbol "fun" ;;
let symProc = symbol "proc" ;;
let symClass = symbol "class";;
let symConst = symbol "const";;
let symNewint = symbol "newint";;
let symRecord = symbol "record";;
let symColon = symbol ":";;
let symLSB = symbol "{";;
let symRSB = symbol "}";;


let semiP dp1 dp2 =  bind (dp1 ()) (fun d1 -> (bind' symSemi (bind (dp2 ()) (fun d2 -> return (Semi(d1,d2))))));;
let commaP dp1 dp2  = bind (dp1 ()) (fun d1 -> (bind' symComma (bind (dp2 ()) (fun d2 -> return (Comma(d1,d2))))));;

let varP tsP = bind' symVar (bind identifier (fun i -> bind' symColon (bind (tsP ()) (fun ts -> return (Var (i,ts))))));;
let funP = bind' symFun (bind identifier (fun i -> bind' symEq (bind expressionP (fun e -> return (Fun(i,e))))));;
let procP = bind' symProc (bind identifier (fun i -> bind' symEq (bind commandP (fun c -> return (Proc(i,c))))));;
let classP tsP = bind' symClass (bind identifier (fun i -> bind' symEq (bind (tsP ()) (fun ts -> return (TClass(i,ts))))));;
let constP = bind' symConst (bind identifier (fun i -> bind' symEq (bind expressionP (fun e -> return (Const(i,e))))));;
let recordP dP = bind' symRecord (bind' symLSB (bind (dP ()) (fun d -> bind' symRSB (return (Record(d))))));;
let newintP = bind' symNewint (return Newint);;
let idStructP = bind identifierP (fun i -> return (X i));;

let (declarationP,type_structureP) =
  let rec dP _ = 
    let rec declP _ = 
      choices [semiP declP' declP; commaP declP' declP; declP' ()]
    and declP' _ = 
      choices [varP tsP; funP; procP; constP; classP tsP; wrapped declP ]
    in 
      declP ()
  and  tsP _  = choices [newintP; recordP dP; idStructP]
  in
    (dP (), tsP()) 
;;
   

(* some tests *)

(* parse declarationP  "var x: newint; var y: X";; *)
(* parse declarationP "fun f = @x ";; *)
(* parse declarationP "var x:newint ; fun f = @x ";; *)
(* parse declarationP "var x :newint , fun f = @x ";; *)
(* parse declarationP "fun a = @x + 1; fun b = a + a";; *)
(* parse declarationP "fun f = @x; proc P = x := X + 19;var x : M";; *)
(* parse type_structureP "X.i";; *)
(* parse type_structureP "record {var x: newint; var y: M}";; *)
(* parse type_structureP "record {var x: newint, var y: M}";; *)
(* parse declarationP "class xy = record {var x: newint, var y: M}";; *)



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
