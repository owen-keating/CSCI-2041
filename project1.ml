(*
   PRINT POLY. Pretty-print polynomials from CSci 2041 Project 1.
     James Moen
     25 Sep 22
*)

(* PRINTF. Define various predefined printing functions. *)

open Printf ;;
open List ;;

(* POLY. A univariate polynomial, with nonzero integer coefficients and integer
   exponents greater than or equal to zero. *)

type poly =
  PolyEmpty |
  PolyTerm of int * int * poly ;;

exception MakePolyError ;;

let compare e p =
  match p with PolyEmpty -> true
    | PolyTerm(coef, expo, next) ->
        e > expo ;;

let rec isPolyOk p =
  match p with PolyEmpty -> true
    | PolyTerm(coef, expo, next) ->
        if coef != 0 && expo >= 0 && compare expo next
          then isPolyOk next
        else false ;;

let rec makePoly i =
  if i = []
    then PolyEmpty
  else if (tl i) = []
    then raise MakePolyError
  else let lst = (tl i) in
    if (tl lst) = []
      then PolyTerm((hd i), (hd lst), PolyEmpty)
    else if isPolyOk (PolyTerm((hd i), (hd lst), makePoly (tl lst)))
      then PolyTerm((hd i), (hd lst), makePoly(tl lst))
    else raise MakePolyError ;;

let rec polyAdd l r =
  match (l, r) with (PolyEmpty, PolyEmpty) -> PolyEmpty
    | (PolyTerm(l_coef, l_expo, l_next), PolyEmpty) -> l
    | (PolyEmpty, PolyTerm(r_coef, r_expo, r_next)) -> r
    | (PolyTerm(l_coef, l_expo, l_next), PolyTerm(r_coef, r_expo, r_next)) ->
        if(l_expo = r_expo)
          then PolyTerm(l_coef + r_coef, l_expo, polyAdd l_next r_next)
        else if(l_expo < r_expo)
          then PolyTerm(r_coef, r_expo, polyAdd l r_next)
        else PolyTerm(l_coef, l_expo, polyAdd l_next r) ;;

let rec polyMinus r =
  match r with PolyEmpty -> PolyEmpty
    | PolyTerm(coef, expo, next) ->
        PolyTerm(-coef, expo, polyMinus next) ;;

(* PRINT POLY. Print POLY so it's allegedly easy to read. You need not know how
   this works. Maybe it will help with debugging. *)

let printPoly poly =
(* PRINTING POLY. Do all the work for PRINT POLY. *)
  let rec printingPoly poly =
    match poly
    with PolyEmpty ->
          () |
         PolyTerm (coef, expo, other) ->
          printf " %c %i x^%i"
            (if coef < 0 then '-' else '+')
            (abs coef) expo ;
          printingPoly other

(* Lost? This is PRINT POLY's body. *)
  in match poly
     with PolyEmpty ->
            printf "0\n" |
          PolyTerm(coef, expo, other) ->
            printf "%i x^%i" coef expo ;
            printingPoly other ;
            printf "\n" ;;

(* PROJECT 1 UNIT TESTS OWEN KEATING *)

isPolyOk (PolyEmpty) ;;
isPolyOk (PolyTerm(3, 3, PolyTerm(2, 4, PolyTerm(4, 1, PolyEmpty)))) ;;
isPolyOk (PolyTerm(1, 3, PolyTerm(2, 2, PolyTerm(6, 0, PolyEmpty)))) ;;
isPolyOk (PolyTerm(2, 1, PolyTerm(-3, 0, PolyEmpty))) ;;
isPolyOk (PolyTerm(3, 2, PolyTerm(2, -1, PolyEmpty))) ;;
isPolyOk (PolyTerm(2, 2, PolyTerm(3, 2, PolyTerm(5, 1, PolyEmpty)))) ;;

makePoly [] ;;
makePoly [4; 4; 2; 3; 3; 2; 5; 1] ;;
makePoly [-2; 3; 1; 2; -4; 0] ;;

polyAdd (makePoly []) (makePoly [3; 4; -2; 2; 3; 0]) ;;
polyAdd (makePoly [6; 2; 3; 1; 3; 0]) (makePoly [-2; 2; 4; 1; 2; 0]) ;;
polyAdd (makePoly [-3; 2; -1; 1; 4; 0]) (makePoly [5; 4; -4; 3; 6; 2]) ;;

polyMinus (makePoly []) ;;
polyMinus (makePoly [2; 3; 5; 2; -3; 1; -5; 0]) ;;
polyMinus (makePoly [-4; 2; 3; 1; -2; 0]) ;;

(*
   
PROJECT 1 RESULTS OWEN KEATING

type poly = PolyEmpty | PolyTerm of int * int * poly
exception MakePolyError
val compare : int -> poly -> bool = <fun>
val isPolyOk : poly -> bool = <fun>
val makePoly : int List.t -> poly = <fun>
val polyAdd : poly -> poly -> poly = <fun>
val polyMinus : poly -> poly = <fun>
val printPoly : poly -> unit = <fun>
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = false
- : bool = false
- : poly = PolyEmpty
- : poly =
PolyTerm (4, 4, PolyTerm (2, 3, PolyTerm (3, 2, PolyTerm (5, 1, PolyEmpty))))
- : poly = PolyTerm (-2, 3, PolyTerm (1, 2, PolyTerm (-4, 0, PolyEmpty)))
- : poly = PolyTerm (3, 4, PolyTerm (-2, 2, PolyTerm (3, 0, PolyEmpty)))
- : poly = PolyTerm (4, 2, PolyTerm (7, 1, PolyTerm (5, 0, PolyEmpty)))
- : poly =
PolyTerm (5, 4,
 PolyTerm (-4, 3,
  PolyTerm (3, 2, PolyTerm (-1, 1, PolyTerm (4, 0, PolyEmpty)))))
- : poly = PolyEmpty
- : poly =
PolyTerm (-2, 3,
 PolyTerm (-5, 2, PolyTerm (3, 1, PolyTerm (5, 0, PolyEmpty))))
- : poly = PolyTerm (4, 2, PolyTerm (-3, 1, PolyTerm (2, 0, PolyEmpty)))

*)