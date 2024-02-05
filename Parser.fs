
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

let anAtom =
  parser {
    return! map (VName << snd) tokName
    return! map (VNum  << snd) tokNumber
    return! map (VBool << snd) tokBoolean
    return! map (VStr  << snd) tokString
    return! map (VChar << snd) tokChar
  }

(*
  Parse sequence of manipulated atoms.
*)
let rec sequence : parser<token, List<value>> =
  many acted

(*
  Parse one manipulated atom.
*)
and acted : parser<token, value> =
  parser {
    return! quoted
    return! atomic
  }
(*
  Parse quote of atom.
*)
and quoted : parser<token, value> =
  parser {
    let! () = tokQuote
    return! map quote acted
  }

(*
  Parse atom.
*)
and atomic : parser<token, value> =
  parser {
    return! aList
    return! map VAtom anAtom
  }

(*
  Parse list of manipulated atoms.
*)

and aList : parser<token, value> =
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
    return listToCons elems trail
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