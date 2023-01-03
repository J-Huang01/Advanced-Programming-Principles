(*Jiatan Huang & Ziyue Zhuang CSCI2041 lab4*)

open List;;

let choose etc things = 
  let rec choosing things = 
    match things with [] -> () |
    firstThing :: otherThing -> etc firstThing ; choosing otherThing
  in choosing things


let rec allbut things thing = 
  if things = []
    then things
else if thing = (hd things)
  then (tl things)
else
  (hd things) :: allbut (tl things) thing

let permute etc things = 
  let rec permuting permutedThings unpermutedThings = 
    if unpermutedThings = []
      then etc permutedThings
  else
    choose (fun firstThing -> permuting (firstThing :: permutedThings) (allbut unpermutedThings firstThing)) unpermutedThings
  in permuting [] things;;

(*test result*)
(*val choose : ('a -> 'b) -> 'a list -> unit = <fun>
val allbut : 'a list -> 'a -> 'a list = <fun>
val permute : ('a list -> unit) -> 'a list -> unit = <fun>*)

(*val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
[]
- : unit = ()
[1 ; 2]
- : unit = ()
[0 ; 2]
- : unit = ()
[0 ; 1]
- : unit = ()
[0 ; 1 ; 2]
- : unit = ()
- : unit = ()
1 - : unit = ()
0 1 2 
- : unit = ()
[]
- : unit = ()
[0]
- : unit = ()
[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]
- : unit = ()*)