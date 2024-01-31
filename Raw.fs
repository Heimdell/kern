
module Raw

open Input

(*
  LISP-list. Includes
  - ()
  - (1 2 3)
  - (1 2 3 . 4)
*)
type list =
  { kind  : int                    // ( [ {
    elems : sequence               // main elements
    trail : option<input * acted>  // rest-elements
  }

(*
  Sequence of elements.
*)
and sequence = list<input * acted>

(*
  Atom with possible QUOTE or SPLICE manipulators.
*)
and acted =
  | Quoted  of acted
  | Spliced of acted
  | Plain   of value

(*
  Atomic expression.
*)
and value =
  | VName of string
  | VNum  of float
  | VBool of bool
  | VStr  of string
  | VChar of char
  | VList of list

(*
  Opening paren of given type.
*)
let closing (i : int) =
  match i with
  | 1 -> ")"
  | 2 -> "]"
  | _ -> "}"

(*
  Closing paren of given type.
*)
let opening (i : int) =
  match i with
  | 1 -> "("
  | 2 -> "["
  | _ -> "{"

(*
  Pretty-print list.
*)
let rec ppList : list -> string =
  fun list ->
    opening list.kind
      + String.concat " " (List.map (ppActed << snd) list.elems)
      + match list.trail with
        | None   -> ""
        | Some x -> " . " + ppActed (snd x)
      + closing list.kind

(*
  Pretty-print value.
*)
and ppValue : value -> string =
  function
  | VName name -> name
  | VNum  n    -> n.ToString()
  | VBool true -> "#true"
  | VBool _    -> "#false"
  | VStr  n    -> "\"" + n.ToString() + "\""
  | VChar n    -> "#\"" + string(n) + "\""
  | VList n    -> ppList n

(*
  Pretty-print QUOTE/SPLICE-d value.
*)
and ppActed : acted -> string =
  function
  | Quoted  act -> "'" + ppActed act
  | Spliced act -> "," + ppActed act
  | Plain   act ->       ppValue act

(*
  Pretty-print sequence.
*)
let ppSeq : List<input * acted> -> string =
  String.concat "\n\n" << List.map (ppActed << snd)