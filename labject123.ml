(*  
    LABJECT123. The final lab and final project for CSci 2041.

      James Moen
      06 Dec 22

    Insert your code whereever it says REPLACE THIS COMMENT WITH YOUR CODE. You
    have written most of this code before. You need write only a small amount
    of new code to complete this assignment.
*)

open Printf ;; (* Define the function PRINTF. *)

(*  THING. A Lisp object. This is used by all subsequent modules. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

(* ------------------------------------------------------------------------- *)

(* EVALUATORS. EVALUATOR provides a function EVALUATE that evaluates a THING,
   and an exception EVALUATOR ERROR with an error message inside. *)

module type Evaluators =
sig
  val evaluate: thing -> thing ;;
  exception EvaluatorError of string ;;
end ;;

(* EVALUATOR. The Lisp evaluator. *)

module Evaluator: Evaluators =
struct

(* OOPS. Call this with a descriptive error MESSAGE in case of error. *)

  exception EvaluatorError of string ;;

  let oops message =
    raise (EvaluatorError message) ;;

(* ENV GET. Search the ENVIRONMENT ENV for the value of a SYMBOL whose string
   is NAME, and return that value. If we can't find it, then call continuation
   ETC. *)

  let envGet env name etc =
    let rec envGetting env =
      match env
      with [] ->
             etc () |
           (otherName, otherValue) :: otherEnv ->
             if name = otherName
             then otherValue
             else envGetting otherEnv
    in envGetting env ;;

(* ENV MAKE. Return a new empty ENVIRONMENT. *)

  let envMake () =
    [] ;;

(* ENV PUT. Return a new ENVIRONMENT that's like ENV except that a SYMBOL with
   the string NAME is bound to VALUE. *)

  let envPut name value env =
    (name, value) :: env ;;

(* TEE. The SYMBOL T. We use it to mean TRUE when we can't think of anything
   else. *)

  let tee = Symbol "t" ;;

(* GLOBAL. The global environment. It's a variable so DEFINE (see below) can
   change it. Initially it binds SYMBOLs T and NIL. *)

  let global = ref (envMake ()) ;;

  global := envPut "nil" Nil (! global) ;;
  global := envPut "t"   tee (! global) ;;

(* LOOKUP. Return the value of a SYMBOL whose string is NAME. First search the
   local ENVIRONMENT ENV. If we can't find NAME there, then search GLOBAL, the
   global ENVIRONMENT. It's an error if we can't find NAME there either. *)

  let lookup env name =
    envGet env name
      (fun () ->
        envGet (! global) name
          (fun () ->
            oops ("Unbound name " ^ name))) ;;

(* EVALUATING. Dispatcher. Evaluate the Lisp expression THING in the local
   ENVIRONMENT ENV. A CONS is a function call whose CAR must evaluate to either
   a CLOSURE or a PRIMITIVE, and whose CDR is a Lisp list of arguments. A
   SYMBOL is a name with a value in ENV. Anything else evaluates to itself. *)

  let rec evaluating thing env =
    match thing
    with Cons(func, args) ->
           (match (evaluating func env)
            with Closure (pars, body, bodyEnv) ->
                   apply pars args env body bodyEnv |
  
               Primitive howTo ->
                   howTo args env |
                 _ ->
                   oops "Closure or primitive expected") |
         Symbol name ->
           lookup env name |
         _ ->
           thing

(* APPLY. Apply a user function whose parameters are the SYMBOLs in the Lisp
   list PARS, whose arguments are the expressions in the Lisp list ARGS, and
   whose body is the Lisp expression BODY. The arguments are evaluated in the
   ENVIRONMENT ARGS ENV, and the BODY is evaluated in the ENVIRONMENT BODY
   ENV. *)

  and apply pars args argsEnv body bodyEnv =
    let rec applying pars args bodyEnv =
      match (pars, args)
      with (Nil, Nil) ->
             evaluating body bodyEnv |
           (Nil, Cons (_, _)) ->
             oops "More arguments than parameters" |
           (Cons (_, _), Nil) ->
             oops "Fewer arguments than parameters" |
           (Cons (Symbol name, pars), Cons (arg, args)) ->
             applying pars args
               (envPut name (evaluating arg argsEnv) bodyEnv) |
           _ ->
             oops "Bad application"
    in applying pars args bodyEnv ;;

(* EVALUATE. Evaluate THING in the global ENVIRONMENT. *)

  let evaluate thing =
    evaluating thing (envMake ()) ;;

(* MAKE ARITHMETIC. Return an OCaml function that takes two NUMBER arguments,
   evaluates them both in ENV, and computes a new NUMBER from them using the
   OCaml function OP. If it doesn't work then assert an error MESSAGE. *)

  let makeArithmetic op message =
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Number left, Number right) ->
                           Number (op left right) |
                         _ ->
                           oops message) |
           _ ->
             oops message) ;;

(* MAKE RELATION. Return an OCaml function that takes two NUMBER arguments,
   evaluates them both in ENV, compares them using the OCaml function OP, and
   returns either NIL or T. If it doesn't work then assert an error MESSAGE. *)

  let makeRelation op message =
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Number left, Number right) ->
                           if op left right
                           then tee
                           else Nil |
                         _ ->
                           oops message) |
           _ ->
             oops message) ;;

(* PRIMITIVE. Bind a symbol with the string NAME to a PRIMITIVE that contains
   the OCaml function HOW TO. *)

  let primitive name howTo =
    global := envPut name (Primitive howTo) (! global) ;;

(* *. Multiply two NUMBERs and return a NUMBER. We must write the name of the
   OCaml multiplication function with extra blanks so it won't be read as a
   comment. *)

  primitive "*" (makeArithmetic ( * ) "* expected two numbers") ;;

(* +. Add two NUMBERs and return a NUMBER. *)

  primitive "+" (makeArithmetic (+) "+ expected two numbers") ;;

(* -. Negate a single NUMBER, or subtract two NUMBERs, returning a NUMBER. *)

  primitive "-"
    (fun args env ->
      match args
      with Cons (right, Nil) ->
             (match (evaluating right env)
              with Number right ->
                     Number (- right) |
                   _ ->
                     oops "- expected a number") |
           Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (Number left, Number right) ->
                           Number (left - right) |
                         _ ->
                           oops "- expected two numbers") |
           _ ->
             oops "- expected one or two numbers") ;;

(* /. Divide two NUMBERs and return a NUMBER. If the second NUMBER is 0 then we
   assert an error instead. *)

  primitive "/"
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             let left = evaluating left env
             in let right = evaluating right env
                in (match (left, right)
                    with (_, Number 0) ->
                           oops "/ tried to divide by 0" |
                         (Number left, Number right) ->
                           Number (left / right) |
                         _ ->
                           oops "/ expected two numbers") |
           _ ->
             oops "/ expected two numbers") ;;

(* <, <=, <>, >, >=. Comparisons that test two NUMBERs. *)

  primitive "<"  (makeRelation (<)  "< expected two numbers") ;;
  primitive "<=" (makeRelation (<=) "<= expected two numbers") ;;
  primitive "<>" (makeRelation (<>) "<> expected two numbers") ;;
  primitive ">"  (makeRelation (>)  "> expected two numbers") ;;
  primitive ">=" (makeRelation (>=) "< expected two numbers") ;;

(* "=". Test if two THINGs other than Lisp lists are equal. *)

  primitive "="
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (let left = evaluating left env
              in let right = evaluating right env
                 in match (left, right)
                    with (Nil, Nil) ->
                           tee |
                         (Number left, Number right) ->
                           if left = right
                           then tee
                           else Nil |
                         (Symbol left, Symbol right) ->
                           if left = right
                           then tee
                           else Nil |
                         _ ->
                           Nil) |
           _ ->
             oops "= expected two arguments") ;;

(* AND. If there are no arguments then return T. Otherwise evaluate arguments
   left to right. If one returns NIL then return NIL without evaluating the
   others. Otherwise return the result of evaluating the last argument. *)

  primitive "and"
    (fun args env ->
      let rec anding args =
        match args
        with Nil ->
               tee |
             Cons (arg, Nil) ->
               evaluating arg env |
             Cons (arg, args) ->
               if (evaluating arg env) = Nil
               then Nil
               else anding args |
             _ ->
               oops "AND expected zero or more arguments"
      in anding args) ;;

(* CAR. Return the first element of a nonemtpy Lisp list. *)

  primitive "car"
   (fun args env ->
     match args
     with Cons (arg, Nil) ->
            (match (evaluating arg env)
             with Cons (first, _) ->
                   first |
                  _ ->
                    oops "CAR expected a CONS") |
          _ ->
            oops "CAR expected a CONS") ;;

(* CDR. Return a Lisp list without its first element. *)

  primitive "cdr"
   (fun args env ->
     match args
     with Cons (arg, Nil) ->
            (match (evaluating arg env)
             with Cons (_, rest) ->
                    rest |
                  _ ->
                    oops "CDR expected a CONS") |
          _ ->
            oops "CDR expected a CONS") ;;

(* CONS. Return a new Lisp list whose first element is the first argument, and
   whose remaining elements are in the second argument. *)

  primitive "cons"
    (fun args env ->
      match args
      with Cons (first, Cons (rest, Nil)) ->
             let first = evaluating first env
             in let rest = evaluating rest env
                in (match rest
                    with Cons (_, _) | Nil ->
                           Cons (first, rest) |
                         _ ->
                           oops "CONS expected an object and a list") |
           _ ->
             oops "CONS expected an object and a list") ;;

(* DEFINE. In the GLOBAL environment, bind the first argument, an unevaluated
   SYMBOL, to the result of evaluating the second argument. Finally return the
   SYMBOL. *)

  primitive "define"
    (fun args env ->
      match args
      with Cons (left, Cons (right, Nil)) ->
             (match left
              with Symbol name ->
                     global := envPut name (evaluating right env) (! global) ;
                     left |
                   _ ->
                     oops "DEFINE expected a symbol and an expression") |
           _ ->
             oops "DEFINE expected a symbol and an expression") ;;

(* IF. Accept three arguments: TEST, WHEN TRUE, and WHEN FALSE. First evaluate
   TEST. If it returns NIL, then evaluate the result of evaluating WHEN FALSE.
   Otherwise return the result of evaluating WHEN TRUE. *)

  primitive "if"
    (fun args env ->
      match args
      with Cons (test, Cons (whenTrue, Cons (whenFalse, Nil))) ->
             if evaluating test env = Nil
             then evaluating whenFalse env
             else evaluating whenTrue env |
           _ ->
             oops "IF expected three arguments") ;;

(* IS-CONS. Test if the single argument is a CONS, i.e., a nonempty list. *)

  primitive "is-cons"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Cons (_, _) ->
                     tee |
                   _ ->
                     Nil) |
           _ ->
             oops "IS-CONS expected one argument") ;;

(* IS-FUNCTION. Test if the single argument is a CLOSURE or a PRIMITIVE. *)

  primitive "is-function"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Closure (_, _, _) | Primitive (_) ->
                     tee |
                   _ ->
                     Nil) |
           _ ->
             oops "IS-FUNCTION expected one argument") ;;

(* IS-NUMBER. Test if the single argument is a NUMBER. *)

  primitive "is-number"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Number _ ->
                     tee |
                   _ ->
                     Nil) |
           _ ->
             oops "IS-NUMBER expected one argument") ;;

(* IS-SYMBOL. Test if the single argument is NIL or a SYMBOL. *)

  primitive "is-symbol"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             (match (evaluating arg env)
              with Nil | Symbol _ ->
                     tee |
                   _ ->
                     Nil) |
           _ ->
             oops "IS-SYMBOL expected one argument") ;;

(* IS MEMBER. Helper for ARE PARAMETERS. Test if THING is a member of the Lisp
   list THINGS. *)

  let isMember thing things =
    let rec isMembering things =
      match things
      with Cons (first, rest) ->
             thing = first || isMembering rest |
           _ ->
             false
    in isMembering things ;;

(* ARE PARAMETERS. Helper for LAMBDA. Test if THINGS is a Lisp list of SYMBOLS,
   in which no SYMBOL appears more than once. *)

  let rec areParameters things =
    match things
    with Nil ->
           true |
         Cons (first, rest) ->
           (match first
            with Symbol _ ->
                   not (isMember first rest) && (areParameters rest) |
                 _ ->
                   false) |
         _ ->
           false ;;

(* LAMBDA. Return a closure. Its parameter list is the unevaluated first
   argument, which mist be a Lisp list of distinct SYMBOLs. Its body is the
   unevaluated second argument. If the current environment is GLOBAL, then the
   closure's ENVIRONMENT is empty. Otherwise its ENVIRONMENT is ENV. *)

  primitive "lambda"
    (fun args env ->
      match args
      with Cons (pars, Cons (body, Nil)) ->
             if areParameters pars
             then Closure (pars, body,
                    (if env == (! global)
                     then envMake ()
                     else env))
             else oops "LAMBDA expected parameters and a body" |
           _ ->
             oops "LAMBDA expected parameters and a body") ;;

(* LIST. Return a Lisp list whose elements are the result of evaluating all the
   arguments. If there are no arguments then return NIL. *)

  primitive "list"
    (fun args env ->
      let rec listing args =
        match args
        with Nil ->
               Nil |
             Cons (arg, args) ->
               Cons (evaluating arg env, listing args) |
             _ ->
               oops "LIST expected zero or more arguments"
      in listing args) ;;

(* NOT. Test if the single argument is NIL. *)

  primitive "not"
    (fun args env ->
      match args
      with Cons (arg, Nil) ->
             if (evaluating arg env) = Nil
             then tee
             else Nil |
           _ ->
             oops "NOT expected one argument") ;;

(* OR. If there are no arguments then return NIL. Otherwise evaluate arguments
   left to right. If one returns a value other than NIL then return that value,
   without evaluating the others. Otherwise return the result of evaluating the
   last argument. *)

  primitive "or"
   (fun args env ->
     let rec oring args =
       match args
       with Nil ->
              Nil |
            Cons (arg, Nil) ->
              evaluating arg env |
            Cons (arg, args) ->
              let value = evaluating arg env
              in if value = Nil
                 then oring args
                 else value |
            _ ->
              oops "OR expected zero or more arguments"
     in oring args) ;;

(* QUOTE. Return the single argument without evaluating it. *)

  primitive "quote"
    (fun args _ ->
      match args
      with Cons (arg, Nil) ->
             arg |
           _ ->
             oops "QUOTE expected one argument") ;;
end ;;

(* ------------------------------------------------------------------------- *)

(* SCANNER. Lexical scanner for a subset of Lisp, from Lab 9. We didn't make a
   MODULE TYPE for it. It provides TOKENs and MAKE SCANNER, a function that
   returns a new token scanner. *)

module Scanner =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)

  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)

  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)

  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in

(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)

  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
         _ ->
           nextSymbolToken ""

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end (* Scanner. *)

(* ------------------------------------------------------------------------- *)

(* PARSERS. PARSER provides an exception CAN'T PARSE and a function MAKE PARSER
   that returns a new parser. *)

module type Parsers =
sig
  exception Can'tParse of string
  val makeParser : string -> unit -> thing
end ;;

(* PARSER. Read Lisp expressions from a file. *)

module Parser: Parsers =
struct

  exception Can'tParse of string ;;

  let makeParser path =
    let nextToken = Scanner.makeScanner path in
    let token = ref (nextToken()) in
    let rec nextThing () =
      let rec nextThings () =
        match !token with
          Scanner.CloseParenToken ->
            token := nextToken();
            Nil
        | Scanner.EndToken ->
            raise (Can'tParse "Unexpected end of file")
        | _ ->
            let first = nextThing() in
            Cons(first, nextThings())
      in
      match !token with
        Scanner.CloseParenToken ->
          raise (Can'tParse "Unexpected closing parentheses")
      | Scanner.EndToken ->
          raise (Can'tParse "Unexpected end of file")
      | Scanner.NumberToken n ->
          token := nextToken();
          Number n
      | Scanner.OpenParenToken ->
          token := nextToken();
          nextThings()
      | Scanner.SymbolToken "nil" ->
          token := nextToken();
          Nil
      | Scanner.SymbolToken s ->
          token := nextToken();
          Symbol s
    in nextThing ;;

end ;;

(* ------------------------------------------------------------------------- *)

(* PRINTERS. PRINTER provides an exception BAD THING and a function PRINT THING
   that prints a Lisp THING. *)

module type Printers =
sig
  exception BadThing
  val printThing : thing -> unit
end ;;

(* PRINTER. Print Lisp THINGs to standard output. *)

module Printer: Printers =
struct

  exception BadThing ;;

  let printThing thing =
    let rec printingThing thing =
      let rec printingThings things =
        match things with
            Cons(first, rest) ->
              printf " " ;
              printingThing first ; 
              printingThings rest
          | Nil -> ()
          | _ -> raise BadThing
      in
      match thing with
          Closure(_, _, _) -> printf "[Closure]"
        | Cons(first, rest) ->
            printf "(" ;
            printingThing first ;
            printingThings rest ;
            printf ")"
        | Nil -> printf "nil"
        | Number(x) -> printf "%i" x
        | Primitive(_) -> printf "[Primitive]"
        | Symbol(str) -> printf "%s" str
    in printingThing thing ;
    printf "\n" ;;

end ;;

(* ------------------------------------------------------------------------- *)

(* LISPERS. Functions visible in the module LISP. It provides only REPL, which
   is a Read-Evaluate-Print-Loop for the Lisp interpreter. *)

module type Lispers =
sig
  val repl : unit -> unit
end ;;

(* LISP. Read Lisp programs from files and execute them. *)

module Lisp: Lispers =
struct

(* COMMAND ARGUMENTS. Call the continuation ETC on every STRING argument from
   the command line, one at a time, left to right. You need not know how this
   function works. Like many things in OCaml, it is far more complicated than
   it should be. *)

  let commandArguments etc =
    Arg.parse
      [ ("", (Arg.String (fun _ -> ())), "Zero or more arguments expected") ]
      etc
      "Argument expected" ;;

  let rec repl () =
    commandArguments (fun path ->
      try (
        let nextThing = Parser.makeParser path in
        let rec repling thing =
          match thing with
            Symbol "end" -> ()
          | _ -> Printer.printThing (Evaluator.evaluate thing) ;
            repling (nextThing())
        in repling (nextThing())
      ) with
        Evaluator.EvaluatorError s -> printf "%s: evaluator error %s\n" path s
      | Parser.Can'tParse s -> printf "%s: parser error %s\n" path s
      | Printer.BadThing -> printf "%s: printer error\n" path
      | _ -> printf "%s: internal error\n" path) ;;

end ;;

(* ------------------------------------------------------------------------- *)

(* The big finish. Run it! *)

Lisp.repl () ;;

(*

LABJECT 123 RESULTS OWEN KEATING

solve
solving
solving-add
solving-subtract
solving-multiply
solving-divide
is-inside
make-op
op
left
right
t
nil
t
nil
nil
t
(= x (- c a))
(= x (- c b))
(= x (+ b c))
(= x (/ c a))
(= x (/ c b))
(= x (/ a c))
(= x (* b c))
(= x (/ (- y b) m))
(= a (* e (+ f (/ (+ b c) d))))

*)*)*)
