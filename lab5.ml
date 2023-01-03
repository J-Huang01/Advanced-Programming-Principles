(*CSCI 2041 lab5 Jiatan Huang*)
let makeStream this state next =
  ((this, state), next) ;;

(* FIRST. Return the first element of a stream. *)

let first ((this, state), next) =
  this ;;

(* REST. Return a stream with its first element removed. *)

let rest ((this, state), next) =
  (next this state, next) ;;

let odds = 
  makeStream 1 () (fun this state ->(this+2, ()));;

let rec trim count stream = 
  match count
with 1 -> rest stream |
    _ -> trim (count - 1)(rest stream);;

let scale factor stream =
  makeStream (factor * (first stream)) (rest stream) (fun this state -> (factor*(first state), rest state));;

let sum left right =
  makeStream (first left + first right) (rest left, rest right) (fun this (left,right) -> ((first left + first right), (rest left, rest right)));;

(*test result*)

(*val makeStream : 'a -> 'b -> 'c -> ('a * 'b) * 'c = <fun>
val first : ('a * 'b) * 'c -> 'a = <fun>
val rest : ('a * 'b) * ('a -> 'b -> 'c) -> 'c * ('a -> 'b -> 'c) = <fun>
val take : int -> ('a * 'b) * ('a -> 'b -> 'a * 'b) -> 'a list = <fun>
val naturals : (int * unit) * (int -> '_weak42 -> int * unit) =
  ((0, ()), <fun>)
- : int = 1
- : int = 3
- : int = 5
- : int list = [1; 3; 5; 7; 9; 11; 13]
val but1st5 : (int * unit) * (int -> unit -> int * unit) = ((5, ()), <fun>)
- : int = 5
- : int = 6
- : int = 7
- : int list = [5; 6; 7; 8; 9; 10; 11]
val byFives :
  (int * ((int * unit) * (int -> unit -> int * unit))) *
  ('_weak43 ->
   (int * '_weak44) * (int -> '_weak44 -> '_weak45) ->
   int * ('_weak45 * (int -> '_weak44 -> '_weak45))) =
  ((0, ((1, ()), <fun>)), <fun>)
- : int = 0
- : int = 5
- : int = 10
- : int list = [0; 5; 10; 15; 20; 25; 30]
val natsPlusByFives :
  (int *
   (((int * unit) * (int -> unit -> int * unit)) *
    ((int * ((int * unit) * (int -> unit -> int * unit))) *
     (int ->
      (int * unit) * (int -> unit -> int * unit) ->
      int * ((int * unit) * (int -> unit -> int * unit)))))) *
  ('_weak46 ->
   ((int * '_weak47) * (int -> '_weak47 -> '_weak48)) *
   ((int * '_weak49) * (int -> '_weak49 -> '_weak50)) ->
   int *
   (('_weak48 * (int -> '_weak47 -> '_weak48)) *
    ('_weak50 * (int -> '_weak49 -> '_weak50)))) =
  ((0, (((1, ()), <fun>), ((5, ((2, ()), <fun>)), <fun>))), <fun>)
- : int = 0
- : int = 6
- : int list = [0; 6; 12; 18; 24; 30; 36]*)
