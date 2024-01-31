
module rec Parser

open Parsing
open Input
open Lexer
open Raw

(*
  Various tokens.
*)

let tokName : parser<token, input * string> =
  deco "name" << satisfy <|
    function
    | Name name -> Some name
    | _         -> None

let tokChar : parser<token, input * char> =
  deco "char" << satisfy <|
    function
    | Ch ch -> Some ch
    | _     -> None

let tokString : parser<token, input * string> =
  deco "string" << satisfy <|
    function
    | Str str -> Some str
    | _       -> None

let tokNumber : parser<token, input * float> =
  deco "number" << satisfy <|
    function
    | Num num -> Some num
    | _       -> None

let tokBoolean : parser<token, input * bool> =
  deco "boolean" << satisfy <|
    function
    | Bool num -> Some num
    | _         -> None

let tokOpen : parser<token, input * int> =
  deco "'(', '[', '{'" << satisfy <|
    function
    | Open i -> Some i
    | _      -> None

let tokClose (i : int) : parser<token, unit> =
  deco ("'" + closing i + "'") << map snd << satisfy <|
    function
    | Close j when i = j -> Some ()
    | _                  -> None

let tokDot : parser<token, unit> =
  deco "'.'" << map snd << satisfy <|
    function
    | Dot -> Some ()
    | _   -> None

let tokQuote : parser<token, unit> =
  deco "quote" << map snd << satisfy <|
    function
    | Quote -> Some ()
    | _     -> None

let tokSplice : parser<token, unit> =
  deco "','" << map snd << satisfy <|
    function
    | Splice -> Some ()
    | _      -> None

let dump : parser<token, tokens<token>> =
  fun input ->
    { expected = Set []
      output   = None
      produce  = Some input
    }

(*
  Parse sequence of manipulated atoms.
*)
let rec sequence : parser<token, List<input * acted>> =
  many acted

(*
  Parse one manipulated atom.
*)
and acted : parser<token, input * acted> =
  parser {
    return! spliced
    return! quoted
    return! map (second Plain) atomic
  }

(*
  Parse splice of atom.
*)
and spliced : parser<token, input * acted> =
  parser {
    let! () = tokSplice
    return! map (second Spliced) acted
  }

(*
  Parse quote of atom.
*)
and quoted : parser<token, input * acted> =
  parser {
    let! () = tokQuote
    return! map (second Quoted) acted
  }

(*
  Parse atom.
*)
and atomic : parser<token, input * value> =
  parser {
    return! map (second VList) aList
    return! map (second VName) tokName
    return! map (second VNum)  tokNumber
    return! map (second VBool) tokBoolean
    return! map (second VStr)  tokString
    return! map (second VChar) tokChar
  }

(*
  Parse list of manipulated atoms.
*)

and aList : parser<token, input * list> =
  parser {
    let! pos, i = tokOpen
    let! elems, trail =
      option ([], None) <| parser {
        let! elems  = some acted
        let! trail  =
          optional <| parser {
            let! () = tokDot
            return! acted
          }
        return elems, trail
      }
    let! ()     = tokClose i
    return pos,
      { kind  = i
        elems = elems
        trail = trail
      }
  }

(*
  Run parser.
*)
let testParser
  (ma    : parser<token, 'a>)  // parser to run
  (input : input)              // source
  (k     : 'a -> 'b)           // what to do on success
         : option<'b> =
  let toks, rest = tokens input
  if Input.eos rest
  then
    let a = ma toks
    match a.produce with
    | Some a -> Some (k a)
    | None   ->
      printfn "%s"
        <| report (pos a.output input)
         + "expected " + toText a.expected
      None
  else
    printfn "%s"
      <| report rest + "Cannot tokenize."
    None