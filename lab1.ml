open List;;

(*1. How Many*)
let rec howMany e l =
  if l = []
    then 0
  else if e = (hd l)
    then 1 + howMany e (tl l)
  else
    howMany e (tl l);;

(*2.Delete*)
let rec delete e l =
  if l = []
    then l
  else if e = (hd l)
    then delete e (tl l)
  else
    (hd l) :: delete e (tl l);;

(*3.Mean*)
let mean l=
  let rec length l = 
    if l = []
      then 0.0
    else
      length (tl l) +. 1.0
  in
  let rec sum l=
  if l = []
    then 0.0
  else sum (tl l) +. (hd l)
  in 
  sum l /. length l;;

  (*test result*)

  (*# #use "dromedary.ml";;
val howMany : 'a -> 'a List.t -> int = <fun>
val delete : 'a -> 'a List.t -> 'a List.t = <fun>
val mean : float List.t -> float = <fun>*)

(*val printThings : ('a -> 'b, out_channel, unit) format -> 'a List.t -> unit =
  <fun>
0
- : unit = ()
1
- : unit = ()
1
- : unit = ()
0
- : unit = ()
2
- : unit = ()
0
- : unit = ()
[]
- : unit = ()
[]
- : unit = ()
[2 ; 3]
- : unit = ()
[1 ; 2 ; 3]
- : unit = ()
[2 ; 3 ; 4]
- : unit = ()
[x ; y]
- : unit = ()
1.000000
- : unit = ()
1.500000
- : unit = ()
0.250000
- : unit = ()*)

