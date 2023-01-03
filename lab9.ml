(*CSCI 2041 Lab9 Jiatan Huang && Ziyue Zhuang*)

open Printf;;

exception Error;;

type thing = 
  Closure of thing * thing * environment | 
  Cons of thing * thing | 
  Nil | 
  Number of int | 
  Primitive of (thing -> environment -> thing) | 
  Symbol of string 
and 
  environment = (string * thing) list ;;

let rec printingThing thing = 
  let rec printingThings things =
    match things with
    Nil -> () |
    Cons(left, right) -> printf " "; printingThing left; printingThings right |
    _ -> raise Error 
  in 
  match thing with 
  Closure (p, b, e) -> printf "[Closure]" |
  Cons (left, right) -> printf "("; printingThing left; printingThings right; printf ")" |
  Nil -> printf "nil" |
  Number n -> printf "%i" n |
  Primitive h -> printf "[Primitive]" |
  Symbol s -> printf "%s" s;;

let printThing thing = 
  printingThing thing;
  printf("\n");;

(*test result*)

(* exception Error
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
val printingThing : thing -> unit = <fun>
val printThing : thing -> unit = <fun>

nil
- : unit = ()
7734
- : unit = ()
lobyms
- : unit = ()
[Closure]
- : unit = ()
[Primitive]
- : unit = ()
(a)
- : unit = ()
(a b)
- : unit = ()
(a b c)
- : unit = ()
((a) b c)
- : unit = ()
((a b) c)
- : unit = ()
(a (b c))
- : unit = ()
((a b c))
- : unit = ()
(define ! (lambda (n) (if (= n 0) 1 (* n (! (- n 1))))))
- : unit = () *)
*)
