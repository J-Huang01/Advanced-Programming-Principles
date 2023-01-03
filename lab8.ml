(*CSCI 2041 Lab8 Jiatan Huang && Ziyue Zhuang*)
open Lazy;;

type 'base s = 
LazyEmpty | LazyNode of 'base Lazy.t * 'base s Lazy.t;;

exception LazyListError;;

let lazyCons h t =
  LazyNode (h, t);;

let lazyHead l =
  match l
with LazyEmpty -> raise LazyListError |
LazyNode(head, _) -> force head;;

let lazyTail l =
  match l 
  with LazyEmpty -> raise LazyListError |
LazyNode(_, tail) -> force tail;;

(*this is a helper function that reverse the list*)

let reverse l = 
  let rec reversing newL = function 
    | [] -> newL 
    | hd :: tl -> reversing (hd::newL) tl
  in reversing [] l;;

let lazyTake l n = 
  let rec taking newL l n =
    match l with
    |LazyEmpty->if n > 0
      then raise LazyListError
  else newL
  |LazyNode(head,tail) -> 
  match n with 
  | 0 -> newL
  | _ -> taking (lazyHead l::newL) (lazyTail l) (n-1) 
  in reverse(taking [] l n);;


(*test result*)
(*type 'base s = LazyEmpty | LazyNode of 'base lazy_t * 'base s lazy_t
exception LazyListError
val lazyCons : 'a lazy_t -> 'a s lazy_t -> 'a s = <fun>
val lazyHead : 'a s -> 'a = <fun>
val lazyTail : 'a s -> 'a s = <fun>
val reverse : 'a list -> 'a list = <fun>
val lazyTake : 'a s -> int -> 'a list = <fun>*)

(*val lazyInts : int -> int -> int s = <fun>
val lazyFibs : unit -> int s = <fun>
val strings : string s = LazyNode (lazy "I'm", <lazy>)
- : string = "I'm"
- : string = "so"
- : string = "lazy"
Oops.
- : string = ""
- : string list = ["I'm"; "so"; "lazy"]
val oneThruNine : int s = LazyNode (<lazy>, <lazy>)
Computed integer 1
Computed integer 2
Computed integer 3
- : int list = [1; 2; 3]
Computed integer 4
Computed integer 5
Computed integer 6
Computed integer 7
Computed integer 8
Computed integer 9
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
val allTheFibs : int s = LazyNode (<lazy>, <lazy>)
Computed Fibonacci 0
Computed Fibonacci 1
Computed Fibonacci 1
Computed Fibonacci 2
Computed Fibonacci 3
Computed Fibonacci 5
Computed Fibonacci 8
Computed Fibonacci 13
Computed Fibonacci 21
Computed Fibonacci 34
- : int list = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]
- : int list = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]*)