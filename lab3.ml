(*Jiatan Huang & Ziyue Zhuang
   CSCI2041 Lab4*)
type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;

exception BadEmptyBst;;

let rec bstMaxKey tree = 
  match tree
with BstEmpty -> raise BadEmptyBst | 
BstNode(key, _, BstEmpty) -> key |
BstNode(_, _, rightSubtree) -> bstMaxKey rightSubtree;;

let rec bstDelete tree key = 
  match tree
with BstEmpty -> BstEmpty | (*deleting a key from an empty BST*)

BstNode(otherKey, BstEmpty, BstEmpty) -> BstEmpty | (*deleting a key from a BST whose root has empty left and right subtrees*)

BstNode(otherKey, BstEmpty, rightSubtree) -> (*deleting a key from a BST whose root has an empty left subtree, and a non-empty roght subtree*)
  if key != otherKey
    then BstNode(otherKey, BstEmpty, (bstDelete rightSubtree key))
  else
    rightSubtree | 

BstNode(otherKey, leftSubtree, BstEmpty) -> (*deleting a key from a BST whose root has a non-empty left subtree, and an empty right subtree*)
  if key != otherKey
    then BstNode(otherKey, (bstDelete leftSubtree key), BstEmpty)  
  else
    leftSubtree | 

BstNode(otherKey,leftSubtree, rightSubtree) ->(*deleting a key from a BST whose root has a non-empty left subtree, and a non-empty right subtree*)
  if key < otherKey
    then BstNode(otherKey, (bstDelete leftSubtree key), rightSubtree)

  else if key > otherKey
    then BstNode(otherKey, leftSubtree, (bstDelete rightSubtree key))

  else 
    let maxVal = bstMaxKey leftSubtree
  in BstNode(maxVal, (bstDelete leftSubtree maxVal), rightSubtree);;


(*test result*)
 (*type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst
exception BadEmptyBst
val bstMaxKey : 'a bst -> 'a = <fun>
val bstDelete : 'a bst -> 'a -> 'a bst = <fun>*)

(*val bstInsert : 'a bst -> 'a -> 'a bst = <fun>
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
     BstNode (997, BstEmpty, BstEmpty))))*)


(*5 points*)
(*val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstEmpty, BstNode (997, BstEmpty, BstEmpty))))*)

(*5 points*)
(*val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))*)

(*5 points*)
(*val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (74, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))*)

(*10 points*)
(*val t : int bst =
  BstNode (74, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))*)

(*5 points*)
(*val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))*)

(*5 points*)
(*val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (212, BstEmpty, BstEmpty))*)


(*val t : int bst = BstNode (74, BstEmpty, BstNode (212, BstEmpty, BstEmpty))
val t : int bst = BstNode (74, BstEmpty, BstEmpty)*)

(*5 points*)
(*val t : int bst = BstEmpty*)