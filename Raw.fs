
module Raw

open Input

(*
  Atomic expression.
*)
type atom =
  | VName of string
  | VNum  of float
  | VBool of bool
  | VStr  of string
  | VChar of char

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
  app "quote" (cons x VNil)

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

let ppAtom : atom -> string =
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

let ppSeq : List<value> -> string = fun s ->
  "("
    + String.concat " " (List.map ppValue s)
    + ")"