(*Rational arithmetic*)

let num = fst;;
let den = snd;;

let rec gcd i j =
  if i <> 0
  then if j > i
      then gcd i (j-i)
      else gcd (i-j) j 
  else j;;


let rat n d =
  (n/gcd n d, d/gcd n d);;

let ratAdd a b =
  rat (num a * den b + den a * num b) (den a * den b);;

let ratMul a b =
  rat (num a * num b) (den a * den b);;

let ratDiv a b =
  rat (num a * den b) (den a * num b);;

let ratGt a b =
if (num a * den b) > (den a * num b)
  then true
else
  false;;
      

let euler() =
  let rec eulering c s t = 

  if ratGt t (ratDiv (1,1) (100000,1))
    then
      let new_s = ratAdd s t
    in
    let new_t = ratDiv t c 
    in
    let new_c = ratAdd c (1,1)
    in
    eulering (new_c) (new_s) (new_t)
  else
    s
  in eulering (rat 1 1) (rat 0 1) (rat 1 1);;

(*test result*)
(*val num : 'a * 'b -> 'a = <fun>
val den : 'a * 'b -> 'b = <fun>
val gcd : int -> int -> int = <fun>
val rat : int -> int -> int * int = <fun>
val ratAdd : int * int -> int * int -> int * int = <fun>
val ratMul : int * int -> int * int -> int * int = <fun>
val ratDiv : int * int -> int * int -> int * int = <fun>
val ratGt : int * int -> int * int -> bool = <fun>
val euler : unit -> int * int = <fun>*)

(*val ratPrint : int * int -> unit = <fun>
val boolPrint : bool -> unit = <fun>
1 / 2
- : unit = ()
1 / 2
- : unit = ()
1 / 1
- : unit = ()
5 / 6
- : unit = ()
5 / 1
- : unit = ()
8 / 15
- : unit = ()
1 / 10
- : unit = ()
3 / 2
- : unit = ()
true
- : unit = ()
false
- : unit = ()
109601 / 40320
- : unit = ()*)
    






