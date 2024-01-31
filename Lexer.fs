
module Lexer

open Lexing
open Input

(*
  Name cannot start from '
*)
let startNameChar = diap 'a' 'z'
                <|> diap 'A' 'Z'
                <|> oneOf "-+=/?!@$%^&*:<>\\|~"

(*
  Name can have ' as any char other than first.
*)
let restNameChar  = diap 'a' 'z'
                <|> diap 'A' 'Z'
                <|> oneOf "-+=/?!@$%^&*:<>\\|~\'"
                <|> digit

let name = startNameChar &&& many restNameChar

(*
  Any other in-string char.
*)
let charLit = noneOf "\\\"" <|> (one '\\' &&& oneOf "ntr\\\"")

(*
  Char literals look like #"\n", string literals look like "th\\is"
*)
let charLiteral   = one '#' &&& one '"' &&&      charLit &&& one '"'
let stringLiteral =             one '"' &&& many charLit &&& one '"'

(*
  All number are floats.
*)
let numberLiteral = some digit &&& Opt (one '.' &&& some digit)

(*
  Separate literals for boolean #true and #false
*)
let trueLit       = one '#' &&& one 't' &&& one 'r' &&& one 'u' &&& one 'e'
let falseLit      = one '#' &&& one 'f' &&& one 'a' &&& one 'l' &&& one 's' &&& one 'e'

(*
  Punctuation.
*)
let open1  = one '('
let open2  = one '['
let open3  = one '{'
let close1 = one ')'
let close2 = one ']'
let close3 = one '}'
let dot    = one '.'
let quote  = one '\''
let splice = one ','

(*
  Token type
*)
type token =
  | Name  of string
  | Str   of string
  | Ch    of char
  | Num   of float
  | Bool  of bool
  | Open  of int  // kind of paren
  | Close of int  // -"-
  | Dot
  | Quote
  | Splice

(*
  Tokenize input.
*)
let tokens : input -> list<input * token> * input =
  tokenise
    [ name          , Name
      stringLiteral , fun s -> Str (s.[1.. String.length s - 2])
      charLiteral   , fun c -> Ch (c[2])
      numberLiteral , fun s -> Num (float s)
      open1         , fun _ -> Open 1
      open2         , fun _ -> Open 2
      open3         , fun _ -> Open 3
      close1        , fun _ -> Close 1
      close2        , fun _ -> Close 2
      close3        , fun _ -> Close 3
      dot           , fun _ -> Dot
      quote         , fun _ -> Quote
      splice        , fun _ -> Splice
      trueLit       , fun _ -> Bool true
      falseLit      , fun _ -> Bool false
    ]
