(*
  CSci 2041 Tests for Lab Assignment 3.

    James Moen
    25 Sep 22

  It's worth 40 points.
*)

(* BST. An unbalanced Binary Search Tree of KEYs. *)

type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;

exception BadEmptyBst ;;

let rec bstMaxKey tree =
  match tree with
      BstEmpty -> raise BadEmptyBst
    | BstNode(head, leftSubtree, rightSubtree) ->
        if rightSubtree = BstEmpty
          then head
        else bstMaxKey rightSubtree ;;

let rec bstDelete tree key =
  match tree with
      BstEmpty -> BstEmpty
    | BstNode(head, BstEmpty, BstEmpty) when key = head -> BstEmpty
    | BstNode(head, BstEmpty, rightSubtree) when key = head -> rightSubtree
    | BstNode(head, leftSubtree, BstEmpty) when key = head -> leftSubtree
    | BstNode(head, leftSubtree, rightSubtree) ->
        if key < head
          then BstNode(head, (bstDelete leftSubtree key), rightSubtree)
        else if key > head
          then BstNode(head, leftSubtree, (bstDelete rightSubtree key))
        else
          let maxValue = bstMaxKey leftSubtree in
          BstNode(maxValue, (bstDelete leftSubtree maxValue), rightSubtree) ;;

(* BST INSERT. Return a new BST that's like TREE, but with KEY. We need this to
   make the test cases below. *)

let bstInsert tree key =
  let rec inserting subtree =
    match subtree
    with BstEmpty -> BstNode(key, BstEmpty, BstEmpty) |
         BstNode(otherKey, leftSubtree, rightSubtree) ->
           if key < otherKey
           then BstNode(otherKey, inserting leftSubtree, rightSubtree)
           else if key > otherKey
                then BstNode(otherKey, leftSubtree, inserting rightSubtree)
                else subtree
  in inserting tree ;;

(* BST IS IN. Test if KEY is in TREE. It may help with debugging. You need not
   call it in the code you will write. *)

let bstIsIn key tree =
  let rec isInning subtree =
    match subtree
    with BstEmpty -> false |
         BstNode(otherKey, leftSubtree, rightSubtree) ->
           if key < otherKey
           then isInning leftSubtree
           else if key > otherKey
                then isInning rightSubtree
                else true
  in isInning tree ;;

(* Let T be a BST. We'll make it by adding nodes one at a time, so OCaml will
   print many intermediate BST's that we don't care about. Ignore those. *)

let t = BstEmpty        ;;
let t = bstInsert t 100 ;;
let t = bstInsert t 70  ;;
let t = bstInsert t 137 ;;
let t = bstInsert t 53  ;;
let t = bstInsert t 86  ;;
let t = bstInsert t 74  ;;
let t = bstInsert t 212 ;;
let t = bstInsert t 149 ;;
let t = bstInsert t 997 ;;

(* This is the one we care about, but OCaml will indent it less clearly.

   BstNode (100,
     BstNode (70,
       BstNode (53, BstEmpty, BstEmpty),
       BstNode (86,
         BstNode (74, BstEmpty, BstEmpty),
         BstEmpty)),
     BstNode (137,
       BstEmpty,
       BstNode (212,
         BstNode (149, BstEmpty, BstEmpty),
         BstNode (997, BstEmpty, BstEmpty))))

   We'll delete nodes from it, one at a time. We'll start by deleting a node
   from a left subtree, with no children. *)

let t = bstDelete t 149 ;;

(* 5 points if you get this.

   BstNode (100,
     BstNode (70,
       BstNode (53, BstEmpty, BstEmpty),
       BstNode (86,
         BstNode (74, BstEmpty, BstEmpty),
         BstEmpty)),
     BstNode (137,
       BstEmpty,
       BstNode (212,
         BstEmpty,
         BstNode (997, BstEmpty, BstEmpty)))) *)

(* Delete a node from a right subtree, with no children. *)

let t = bstDelete t 997 ;;

(* 5 points if you get this.

   BstNode (100,
     BstNode (70,
       BstNode (53, BstEmpty, BstEmpty),
       BstNode (86,
         BstNode (74, BstEmpty, BstEmpty),
         BstEmpty)),
     BstNode (137,
       BstEmpty,
       BstNode (212, BstEmpty, BstEmpty))) *)

(* Delete a node from a right subtree, with one child. *)

let t = bstDelete t 86 ;;

(* 5 points if you get this.

   BstNode (100,
     BstNode (70,
       BstNode (53, BstEmpty, BstEmpty),
       BstNode (74, BstEmpty, BstEmpty)),
     BstNode (137,
       BstEmpty,
       BstNode (212, BstEmpty, BstEmpty))) *)

(* Delete a node with two children. *)

let t = bstDelete t 100 ;;

(* 10 points if you get this. I'm assuming you replaced 100 with the largest
   key 74 in the left subtree, then deleted its node.

   BstNode (74,
     BstNode (70,
       BstNode (53, BstEmpty, BstEmpty),
       BstEmpty),
     BstNode (137,
       BstEmpty,
       BstNode (212, BstEmpty, BstEmpty))) *)

(* Delete a node from the left subtree, with one child. *)

let t = bstDelete t 70 ;;

(* 5 points if you get this.

   BstNode (74,
     BstNode (53, BstEmpty, BstEmpty),
     BstNode (137,
       BstEmpty,
       BstNode (212, BstEmpty, BstEmpty))) *)

(* Delete a node from the right subtree, with one child. *)

let t = bstDelete t 137 ;;

(* 5 points if you get this.

   BstNode (74,
     BstNode (53, BstEmpty, BstEmpty),
     BstNode (212, BstEmpty, BstEmpty)) *)

(* The big finish. Delete all the remaining nodes! *)

let t = bstDelete t 53  ;;
let t = bstDelete t 212 ;;
let t = bstDelete t 74  ;;

(* 5 points if you get BstEmpty in the end. *)

(*

LAB 03 RESULTS OWEN KEATING

type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst
exception BadEmptyBst
val bstMaxKey : 'a bst -> 'a = <fun>
val bstDelete : 'a bst -> 'a -> 'a bst = <fun>
val bstInsert : 'a bst -> 'a -> 'a bst = <fun>
val bstIsIn : 'a -> 'a bst -> bool = <fun>
val t : 'a bst = BstEmpty
val t : int bst = BstNode (100, BstEmpty, BstEmpty)
val t : int bst = BstNode (100, BstNode (70, BstEmpty, BstEmpty), BstEmpty)
val t : int bst =
  BstNode (100, BstNode (70, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstEmpty))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstNode (149, BstEmpty, BstEmpty), BstEmpty)))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstNode (149, BstEmpty, BstEmpty),
     BstNode (997, BstEmpty, BstEmpty))))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstEmpty, BstNode (997, BstEmpty, BstEmpty))))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (74, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (74, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (212, BstEmpty, BstEmpty))
val t : int bst = BstNode (74, BstEmpty, BstNode (212, BstEmpty, BstEmpty))
val t : int bst = BstNode (74, BstEmpty, BstEmpty)
val t : int bst = BstEmpty

*)
