(* copyright 2010, James Caldwell *)
(* Department of Computer Science, University of Wyoming *)

(* This code is adapted to Ocaml from the paper
   Hutton and Meijer, 
   Monadic Parsing in Haskell, 
   Journal of Functional Programming, 
   8(4):437-444, July 1998.

   Also described in 
   Graham Hutton, Programming in Haskell, Cambridge Press 2007 
   Richard Bird, Introduction to Functional Programming, Prentice Hall, 1998

  *)



type 'a parser  = P of (string -> ('a * string) list);;

let parse (P f) s = f s;;

let analyze p s = 
  let parses = List.filter (fun (x,s) -> s = "") (parse p s) in
    try (fst (List.hd parses)) with hd -> raise (Failure "analyze: Parse failure")
;;

let return  v = P(fun i -> [(v,i)]);;
let bind p f = P(fun i ->
                  match parse p i with
                      [] -> []
                    | [(v,out)] -> parse (f v) out
		    | _ -> raise (Failure "bind: not general enough - too many parses.")
               )
;;

(* bind' is like bind but discards the result of p *)
let bind' p q = 
  P (fun i -> 
       match parse p i with
	   [] -> []
	 | [(_,out)] -> parse q out
	 | _ -> raise (Failure "bind: not general enough.")
    )
;;

let mzero = P(fun i -> []);;
let mplus p q = P (fun i ->
                     match  (parse p i) with
                         [] -> parse q i
                       | [(v,out)] -> [(v,out)]
		       | _ -> raise (Failure "mplus: not general enough.")
		  )
;;


let failure = mzero;;

let char2str c = String.make 1 c;;
let shead s = s.[0];;
let rest s = String.sub s 1 ((String.length s) - 1);;

let item =  P (fun i ->
                match i with
                    "" -> []
                  | s -> [(shead s, rest s)])
              ;;


let choice p q = mplus p q ;;

let rec choices l = 
  match l with
      [] -> mzero
    | p::ps -> mplus p (choices ps)
;;

(* chainl1 and chainl are defined in
   Hutton and Meijer, 
   Monadic Parsing in Haskell, 
   Journal of Functional Programming.  *)

let chainl1 p op = 
  let rec rest a = 
    choice 
      (bind op (fun f ->
                  bind p (fun b ->
			    rest (f a b))))
	(return a)
  in
    bind p (fun a -> rest a)
;;

let chainl p op a = choice (chainl1 p op) (return a);;

let sat p =  bind item (fun x -> if p x then return x else failure) ;;

let orP p1 p2 = fun x -> p1 x || p2 x;;

let isDigit c = List.mem c ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'];;
let isLower c = List.mem c ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
let isUpper c = List.mem c ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z']
let isAlpha = (orP isLower isUpper);;
let isSpace c =  (c = ' ');;
let isWhiteSpace c = List.mem c [' ';'\t';'\n'];;

let digit =  sat isDigit ;;
let lower = sat isLower ;;
let upper = sat isUpper ;;
let letter =  sat isAlpha ;;
let char x =  sat (fun y -> x = y);;
let alphanum =  choice (char '_') (sat (orP isAlpha isDigit)) ;;

let rec string s =
 match s with
     "" -> return ""
   | s ->
       let x = shead s in
       let xs = rest s in
         bind (char x) (fun _ -> bind (string xs) (fun _ -> return (char2str x ^  xs)))
;;

let rec many1 p =  bind p (fun v -> bind (many p) (fun vs -> return (char2str v ^ vs)))
   and many p  = mplus (many1 p) (return "") ;;

(* since strings are just lists of characters in Haskell -- many and many1 work on strings and lists *)
(* we add these parsers to allow for parsing lists of things of type a *)

let rec list1P p = bind p (fun v -> bind (listP p) (fun vs -> return (v::vs)))
    and listP p = mplus (list1P p) (return [])
;;

let lowerIdent = bind lower (fun x -> bind (many alphanum) (fun xs -> return (char2str x ^ xs)));;
let upperIdent = bind upper (fun x -> bind (many alphanum) (fun xs -> return (char2str x ^ xs)));;
let ident = bind letter (fun x -> bind (many alphanum) (fun xs -> return (char2str x ^ xs)));;
let nat = bind (many1  digit) (fun xs ->  return (int_of_string xs));;

let int = mplus (bind (char '-') (fun _ -> bind nat (fun n -> return (-n)))) nat;;

let space =  bind (many (sat isSpace)) (fun _ -> return ());;
let whiteSpace =  bind (many (sat (isWhiteSpace))) (fun _ -> return ());;

let token p = bind whiteSpace (fun _ -> bind p (fun v -> bind whiteSpace (fun _ -> return v)));;
let prefix s = bind whiteSpace (fun _ -> bind (string s) (fun _ -> return ()));;

let delimted_by c = 
  bind' (char c) (
  bind (many (sat (fun x -> not (x = c)))) (fun s ->
  bind' (char c)(
  (return s))))
;;

let tokenized_delimted_by c = token (delimted_by c) ;;

let identifier = token ident;;
let natural = token nat;;
let integer = token int;;
let symbol s = token (string s);;


(* some lazy evaluation used here to delay calling the parser argument
   -- this is very subtle if you use bind' for the first bind -- the
   argument p is forced and casues a stack overflow.  The function as the
   second argument to bind delays the evaluation of the remaining and so
   if the symLP fails, then wrapped fails without getting stuck
   evaluating p.
*)
let wrapped  p = 
  bind (symbol "(") (fun _ -> bind (p ()) (fun v -> bind' (symbol ")")  (return v)))
;;
