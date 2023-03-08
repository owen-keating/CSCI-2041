(*
  CSci 2041 Tests for Lab Assignment 2

    James Moen
    19 Sep 22

  These tests are worth 40 points total.
*)

open Printf ;;


let rec gcd i j =
  if i <> 0
    then if j > i
      then gcd i (j-i)
      else gcd (i-j) j
  else j ;;

let rat n d =
  let num = n / gcd n d in
  let den = d / gcd n d in
  (num,den) ;;

let ratAdd a b =
  let num = fst a * snd b + fst b * snd a in
  let den = snd a * snd b in
  rat num den ;;

let ratMul a b =
  let num = fst a * fst b in
  let den = snd a * snd b in
  rat num den ;;

let ratDiv a b =
  let num = fst a * snd b in
  let den = snd a * fst b in
  rat num den ;;

let ratGt a b =
  let den = snd a * snd b in
  let one = den / snd a * fst a in
  let two = den / snd b * fst b in
  if one > two
    then true
  else false ;;
  
let euler () =
  let rec sum c s t =
    if ratGt t (rat 1 100000)
      then let s2 = ratAdd s t in
        let t2 = ratDiv t c in
        let c2 = ratAdd c (rat 1 1) in
      sum c2 s2 t2
    else s
  in
  sum (rat 1 1) (rat 0 1) (rat 1 1) ;;

(* RAT PRINT. Print a pair (N, D) as the fraction N / D. You don't have to know
   how this works. *)

let ratPrint (n, d) =
  Printf.printf "%i / %i\n" n d ;;

(* BOOL PRINT. Print a BOOL B. You don't have to know how this works either. *)

let boolPrint b =
  Printf.printf "%b\n" b ;;

(* Test the rational arithmetic functions. *)

ratPrint (rat 1 2) ;;                                       (* 2 pts: 1 / 2 *)

ratPrint (rat 10 20) ;;                                     (* 2 pts: 1 / 2 *)

ratPrint (ratAdd (rat 1 2) (rat 1 2)) ;;                    (* 2 pts: 1 / 1 *)

ratPrint (ratAdd (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: 5 / 6 *)

ratPrint (ratMul (rat 1 2) (rat 10 1)) ;;                   (* 2 pts: 5 / 1 *)

ratPrint (ratMul (rat 2 3) (rat 4 5)) ;;                    (* 2 pts: 8 / 15 *)

ratPrint (ratDiv (rat 1 2) (rat 10 2)) ;;                   (* 2 pts: 1 / 10 *)

ratPrint (ratDiv (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: 3 / 2 *)

boolPrint (ratGt (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: true *)

boolPrint (ratGt (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: false *)

(* The big finish. Compute E. *)

ratPrint (euler ()) ;;                             (* 20 pts: 109601 / 40320 *)

(*

LAB 02 RESULTS OWEN KEATING

1 / 2
1 / 2
1 / 1
5 / 6
5 / 1
8 / 15
1 / 10
3 / 2
true
false
109601 / 40320

*)