
(*
  Regular expressions done wrong.

  It is not regexps, it is recursive descent, LL(inf).
*)
module Lexing

open Maybe
open Input

type regexp =
  | Plus of regexp           // Kleene plus
  | Opt  of regexp           // ?-operator
  | And  of regexp * regexp  // A B, sequence
  | Or   of regexp * regexp  // A|B, choice
  | Chr  of (char -> bool)   // [...], char set

(*
  Compose two functions (a -> a?) sequentally.
*)
let (>>>)
  (f : 'a -> option<'a>)
  (g : 'a -> option<'a>)
  (x : 'a)
     : option<'a> =
  opt {
    let! a = f x
    let! b = g a
    return b
  }

(*
  Of two functions (a -> a?) try first one. If it fails, run second one.
*)
let (|||)
  (f : 'a -> option<'a>)
  (g : 'a -> option<'a>)
  (x : 'a)
     : option<'a> =
  opt {
    return! f x
    return! g x
  }

(*
  Parse single char, belonging to the set.
*)
let single
  (p     : char -> bool)
  (input : input)
         : option<input> =
  opt {
    let! c, rest = uncons input
    if p c then
      return rest
  }

(*
  Accept part of the input that is covered by regular expr.
*)
let rec parse
  (reg   : regexp)
  (input : input)
         : input option =
  input |>
    match reg with
    | Plus r     -> parse r >>> parse (Opt (Plus r))
    | Opt  r     -> parse r ||| Some
    | And (a, b) -> parse a >>> parse b
    | Or  (a, b) -> parse a ||| parse b
    | Chr  p     -> single p

(*
  [l-h]
*)
let diap (l : char) (h : char) : regexp =
  Chr (fun c -> c >= l && c <= h)

(*
  Operator form for Or.
*)
let (<|>) (p : regexp) (q : regexp) : regexp =
  Or (p, q)

(*
  Parse exact char.
*)
let one (d : char) : regexp = Chr <| (=) d

(*
  Set of all chars from string and it's inverse.
*)
let oneOf  (s : string) : regexp = Chr <| fun c ->      s.Contains(c)
let noneOf (s : string) : regexp = Chr <| fun c -> not (s.Contains(c))

(*
  Operator form for And.
*)
let (&&&) l r = And (l, r)

(*
  Less noisy forms for Kleene plus and Kleene star.
*)
let some = Plus
let many = Opt << Plus

(*
  Parse a char or a comment.
*)
let space = Chr System.Char.IsWhiteSpace
        <|> (one ';' &&& many (noneOf "\n"))

(*
  Skip a char or a comment.
*)
let skipSpaces (input : input) =
  input
    |> parse (many space)
    |> Option.defaultValue input

(*
  Parse decimal digit.
*)
let digit = diap '0' '9'

(*
  Given a lis of pairs (regexp, constructor),
  run regexps until one succeeds and then feed the covered span
  to the corresponding constructor.
*)
let rec lexeme
  (pieces : list<regexp * (string -> 'a)>)
  (input  : input)
          : option<(input * 'a) * input> =
  match pieces with
  | [] -> None
  | (reg, ctor) :: rest ->
    match parse reg input with
    | None      -> lexeme rest input
    | Some rest -> Some ((input, ctor (span input rest)), rest)

(*
  Like "lexeme", but repeat and skip spaces in-between tokens.
*)
let rec tokenise
  (pieces : list<regexp * (string -> 'a)>)
  (input' : input)
          : list<input * 'a> * input =
  let input = skipSpaces input'
  if eos input
  then [] , input
  else
    match lexeme pieces input with
    | None -> [] , input
    | Some (tok, rest) ->
      let toks , finish = tokenise pieces rest
      tok :: toks , finish
