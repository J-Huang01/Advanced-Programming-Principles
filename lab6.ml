(*Jiatan Huang & Ziyue Zhuang CSCI 2041 Lab 6*)
type 'base mutyQueue = 
  MutyQueueNode
  of 'base *
    'base mutyQueue ref *
    'base mutyQueue ref;;

let mutyQueueMake s = 
  let rec h = MutyQueueNode(s, ref h, ref h)(*return the head node, left and right pointers point back*)
in h;;

let mutyQueueEmpty q = 
  match q with
  MutyQueueNode(s, l, r)->
    if (!l == q) && (!r == q)(*empty means both left and right points to head*)
      then true
  else
    false;;

let mutyQueueEnqueue q e =
  match q with
  MutyQueueNode(s, l, r)->
    match !l with
    MutyQueueNode(_, _, leftR) ->
      let newNode = MutyQueueNode(e, ref !l, ref q) in
      l := newNode;(*left of node q is the new node*)
      leftR := newNode;;(*right of the original left node is the new node*)

let mutyQueueDequeue q =
  match q with
  MutyQueueNode(s, l, r)->(*check if q is empty*)
    if mutyQueueEmpty q
      then s
  else
    match !r with
    MutyQueueNode(e, _, rightR)->
      match !rightR with
        MutyQueueNode(_, lastL, _)->
          r := !rightR;(*connect the head right skip one node*)
          lastL := q ;
          e;;(*return e*)

(*test result*)
(*type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val mutyQueueMake : 'a -> 'a mutyQueue = <fun>
val mutyQueueEmpty : 'a mutyQueue -> bool = <fun>
val mutyQueueEnqueue : 'a mutyQueue -> 'a -> unit = <fun>
val mutyQueueDequeue : 'a mutyQueue -> 'a = <fun>
   *)
(*type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val queue : string mutyQueue/2 =
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