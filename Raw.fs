
module Raw

open Input

type atom =
  | VName of string
  | VNum  of float
  | VBool of bool
  | VStr  of string
  | VChar of char


(*
  Atomic expression.
*)
and value =
  | VAtom of atom
  | VCons of cons
  | VNil

and cons =
  { car : value
    cdr : value
  }

let cons (a : value) (d : value) : value =
  VCons
    { car = a
      cdr = d
    }

let app (f : string) (x : value) : value =
  cons (VAtom (VName f)) x

let quote (x : value) : value =
  app "QUOTE" (cons x VNil)

let listToCons (elems : list<value>) (trail : option<value>) : value =
  List.foldBack cons elems (trail |> Option.defaultValue VNil)

let rec consToList (c : cons) : list<value> * option<value> =
  match c.cdr with
  | VNil -> [c.car], None
  | VCons d ->
    let xs, rest = consToList d
    c.car :: xs, rest
  | other -> [c.car], Some other

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

let ppAtom =
  function
  | VName name -> name
  | VNum  n    -> n.ToString()
  | VBool true -> "#true"
  | VBool _    -> "#false"
  | VStr  n    -> "\"" + n.ToString() + "\""
  | VChar n    -> "#\"" + string(n) + "\""

let rec ppValue : value -> string =
  function
  | VAtom a -> ppAtom a
  | VNil    -> "()"
  | VCons c ->
    let xs, trail = consToList c
    "("
      + String.concat " " (List.map ppValue xs)
      + match trail with
        | Some x -> " . " + ppValue x
        | None   -> ""
      + ")"

// (*
//   Pretty-print list.
// *)
// let rec ppList : list -> string =
//   fun list ->
//     opening list.kind
//       + String.concat " " (List.map (ppActed << snd) list.elems)
//       + match list.trail with
//         | None   -> ""
//         | Some x -> " . " + ppActed (snd x)
//       + closing list.kind

// (*
//   Pretty-print value.
// *)
// and ppValue : value -> string =
//   function
//   | VAtom a -> ppAtom a
//   | VList n -> ppList n

// (*
//   Pretty-print QUOTE/SPLICE-d value.
// *)
// and ppActed : acted -> string =
//   function
//   | Quoted  act -> "'" + ppActed act
//   | Spliced act -> "," + ppActed act
//   | Plain   act ->       ppValue act

// (*
//   Pretty-print sequence.
// *)
// let ppSeq : sequence -> string =
//   String.concat "\n\n" << List.map (ppActed << snd)