(*Jiatan Huang && Ziyue Zhuang CSCI 2041 Lab 7*)

let c n k =
  let rec computing n k =
    match k with
    0 -> 1 |
    _ ->
      match n with
      0 -> 0 |
      _ -> computing (n-1) k + computing (n-1) (k-1)
    in computing n k;;


let memyC n k =
  let t = Hashtbl.create ~random:false 1000(*creat a hash table*)
in let rec memyCing n k =
  match Hashtbl.find_opt t (n,k) with
  None -> let v =(*creat a variable to save the value*)
  match k with(*check the value of k*)
  0 -> 1 |
  _ -> match n with(*check the value of n*)
  0 -> 0 |
  _ ->memyCing (n-1) k + memyCing (n-1) (k-1) in Hashtbl.add t (n,k) v;v|(*return the value*)
  Some other -> other
in memyCing n k;;

(*test result*)
(*
val time : string -> (unit -> 'a) -> 'a = <fun>
c test1 0.000002 seconds
- : int = 1
c test2 0.000001 seconds
- : int = 0
c test3 0.000001 seconds
- : int = 1
c test4 0.000000 seconds
- : int = 1
c test5 0.000004 seconds
- : int = 70
c test6 0.000001 seconds
- : int = 10
c test7 25.698988 seconds
- : int = 847660528
memyC test1 0.000009 seconds
- : int = 1
memyC test2 0.000007 seconds
- : int = 0
memyC test3 0.000005 seconds
- : int = 1
memyC test4 0.000008 seconds
- : int = 1
memyC test5 0.000010 seconds
- : int = 70
memyC test6 0.000006 seconds
- : int = 10
memyC test7 0.000060 seconds
- : int = 847660528
   *)
