(*
  CSci 2041 Lab Assignment 6

    James Moen
    18 Oct 21

  It's worth 35 points.
*)

(* MUTY QUEUE. A mutable queue of BASEs, as a circular doubly linked list. The
   type doesn't say that the list is circular or doubly linked. That's done by
   the functions that manipulate MUTY QUEUEs. *)

type 'base mutyQueue =
  MutyQueueNode of
   'base *
   'base mutyQueue ref *
   'base mutyQueue ref ;;

let mutyQueueMake s =
  let rec h = MutyQueueNode(s, ref h, ref h) in h ;;

let mutyQueueEmpty q =
  match q with
    MutyQueueNode(element, leftPtr, rightPtr) ->
      q == !leftPtr && q == !rightPtr ;;

let mutyQueueEnqueue q e =
  match q with MutyQueueNode(head, left, _) ->
    match !left with MutyQueueNode(_, _, right) ->
      let node = MutyQueueNode(e, left, ref q) in
      right := node ;
      left := node ;;

let mutyQueueDequeue q =
  match q with MutyQueueNode(head, _, right) ->
    match !right with MutyQueueNode(pop, _, top) ->
      match !top with MutyQueueNode(_, left, _) ->
        right := !top ;
        left := !right ;
        pop ;;

(* Make a QUEUE whose sentinel is the empty string "" and test it. The comments
   say what each test should return, and how many points you get (if any) for
   successful tests. *)

let queue = mutyQueueMake "" ;;

(* 2 pt. MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>}) *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)

mutyQueueEnqueue queue "A" ;;     (* 1 pt. () *)

mutyQueueEmpty queue ;;           (* 2 pt. false *)

mutyQueueEnqueue queue "B" ;;     (* 1 pt. () *)

mutyQueueEnqueue queue "C" ;;     (* 1 pt. () *)

mutyQueueDequeue queue ;;         (* 5 pt. "A" *)

mutyQueueDequeue queue ;;         (* 5 pt. "B" *)

mutyQueueDequeue queue ;;         (* 5 pt. "C" *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 5 pt. "" *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)

(*
   
LAB 06 RESULTS OWEN KEATING

type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val mutyQueueMake : 'a -> 'a mutyQueue = <fun>
val mutyQueueEmpty : 'a mutyQueue -> bool = <fun>
val mutyQueueEnqueue : 'a mutyQueue -> 'a -> unit = <fun>
val mutyQueueDequeue : 'a mutyQueue -> 'a = <fun>
val queue : string mutyQueue =
  MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>})
- : bool = true
- : string = ""
- : unit = ()
- : bool = false
- : unit = ()
- : unit = ()
- : string = "A"
- : string = "B"
- : string = "C"
- : bool = true
- : string = ""
- : string = ""

*)