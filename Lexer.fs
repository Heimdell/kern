
module Lexer

open Maybe
open Input

type regexp =
  | Plus of regexp
  | Opt  of regexp
  | And  of regexp * regexp
  | Or   of regexp * regexp
  | Chr  of (char -> bool)

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

let (|||)
  (f : 'a -> option<'a>)
  (g : 'a -> option<'a>)
  (x : 'a)
     : option<'a> =
  opt {
    return! f x
    return! g x
  }

let single
  (p     : char -> bool)
  (input : input)
         : option<input> =
  opt {
    let! c, rest = uncons input
    if p c then return rest
  }

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

let diap (l : char) (h : char) : regexp =
  Chr (fun c -> c >= l && c <= h)

let (<|>) (p : regexp) (q : regexp) : regexp =
  Or (p, q)

let one    (d : char)   : regexp = Chr <| (=) d
let oneOf  (s : string) : regexp = Chr <| fun c -> s.Contains(c)
let noneOf (s : string) : regexp = Chr <| fun c -> not (s.Contains(c))

let (&&&) l r = And (l, r)

let some = Plus
let many = Opt << Plus

let space = Chr System.Char.IsWhiteSpace
        <|> (one ';' &&& many (noneOf "\n") &&& one '\n')

let skipSpaces (input : input) =
  input
    |> parse (many space)
    |> Option.defaultValue input

let digit = diap '0' '9'

let startNameChar = diap 'a' 'z'
                <|> diap 'A' 'Z'
                <|> oneOf "-+=/?!@$%^&*:<>\\|~"

let restNameChar  = diap 'a' 'z'
                <|> diap 'A' 'Z'
                <|> oneOf "-+=/?!@$%^&*:<>\\|~\'"
                <|> digit

let name = startNameChar &&& many restNameChar

let charLit = noneOf "\\\""
          <|> (one '\\' &&& oneOf "ntr\\\"")

let charLiteral   = one '#' &&& one '"' &&&      charLit &&& one '"'
let stringLiteral =             one '"' &&& many charLit &&& one '"'

let numberLiteral = some digit &&& Opt (one '.' &&& some digit)

let open1  = one '('
let open2  = one '['
let open3  = one '{'
let close1 = one ')'
let close2 = one ']'
let close3 = one '}'
let dot    = one '.'
let quote  = one '\''
let splice = one ','

type token =
  | Name  of string
  | Str   of string
  | Ch    of char
  | Num   of float
  | Open  of int
  | Close of int
  | Dot
  | Quote
  | Splice

let rec lexeme
  (pieces : list<regexp * (string -> token)>)
  (input  : input)
          : option<(input * token) * input> =
  match pieces with
  | [] -> None
  | (reg, ctor) :: rest ->
    match parse reg input with
    | None      -> lexeme rest input
    | Some rest -> Some ((input, ctor (span input rest)), rest)

let rec tokenise
  (pieces : list<regexp * (string -> token)>)
  (input' : input)
          : list<input * token> * input =
  let input = skipSpaces input'
  if eos input
  then [] , input
  else
    match lexeme pieces input with
    | None -> [] , input
    | Some (tok, rest) ->
      let toks , finish = tokenise pieces rest
      tok :: toks , finish

let tokens : input -> list<input * token> * input =
  tokenise
    [ name          , Name
      stringLiteral , fun s -> Str (s.[1.. -1])
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
    ]

// exception CantLex of int * string

// let skipwsp (input : string) (start : int) : int =
//   match parse (Star (Chr System.Char.IsWhiteSpace)) start input with
//   | None   ->     start
//   | Some a -> a + start

// let lexer
//   (tokens  : list<regexp, string -> 'a>)
//   (offset' : int)
//   (input   : string)
//            : list<'a>
//     =
//   let offset = skipwsp input offset'
//   let rec loop (tokens : list<regexp, 'a>) (offset : int) =
//     match tokens with
//     | [] -> raise <| CantLex (offset, input)
//     | (reg, res) :: regs ->
//       match parse reg offset input with
//       | None   -> loop regs offset
//       | Some a ->
//         let next = skipwsp input (a + start)
//         if a + d + start == input.Length
//         then