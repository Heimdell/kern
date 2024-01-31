
(*
  Recursive descent parser. LL(1) unless you call "rollback".
*)
module Parsing

open Maybe
open Input

(*
  Pack set of expected token names into a nice line.
*)
let toText (errs : Set<string>) : string =
  let rec loop : List<string> -> string =
    function
    | []      -> "?"
    | [x]     -> x
    | [x; y]  -> x + " or " + y
    | x :: xs -> x + ", " + loop xs

  loop (Set.toList errs)

(*
  Binary sum type.
*)
type either<'a, 'b> =
  | Ok  of 'a
  | Err of 'b

(*
  Binary sum type eliminator.
*)
let either (f : 'a -> 'c) (g : 'b -> 'c) : either<'a, 'b> -> 'c =
  function
  | Ok  a -> f a
  | Err b -> g b

(*
  Change both components of the sum.
*)
let bimap (f : 'a -> 'c) (g : 'b -> 'd) : either<'a, 'b> -> either<'c, 'd> =
  either (Ok << f) (Err << g)

(*
  Change both components of the pair.
*)
let bimap2 (f : 'a -> 'c) (g : 'b -> 'd) : 'a * 'b -> 'c * 'd =
  fun (a, b) -> f a, g b

(*
  Change second component of the pair.
*)
let second (g : 'b -> 'd) : 'a * 'b -> 'a * 'd = bimap2 id g

(*
  Input stream.
*)
type tokens<'tok> = list<input * 'tok>

(*
  Parse errors.
*)
type error = Set<string>

(*
  Result of the parse.
*)
type result<'tok, 'a> =
  { output   : option<tokens<'tok>>  // remaining input, if consumed
    expected : error                 // errors/"warnings"
    produce  : option<'a>            // result?
  }

(*
  Parser is a function from input stream into the parse result.
*)
type parser<'tok, 'a> = tokens<'tok> -> result<'tok, 'a>

(*
  Unit parser.
*)
let just (x : 'a) : parser<'tok, 'a> =
  fun input ->
    { produce  = Some x
      expected = Set []
      output   = None
    }

(*
  Zero parser.
*)
let empty : parser<'tok, 'a> =
  fun input ->
    { produce  = None
      expected = Set []
      output   = None
    }

(*
  Parse one token, if it fits the predicate.
*)
let satisfy (pred : 'tok -> option<'a>) : parser<'tok, input * 'a> =
  function
  | [] ->
    { produce  = None
      expected = Set []
      output   = None
    }
  | ((pos, tok) :: rest) as input ->
    match pred tok with
    | Some a ->
      { produce  = Some (pos, a)
        expected = Set []
        output   = Some rest
      }
    | None ->
      { produce  = None
        expected = Set []
        output   = None
      }

(*
  Sequence ma to run bebore the dependent parser k.
*)
let bind (ma : parser<'tok, 'a>) (k : 'a -> parser<'tok, 'b>) : parser<'tok, 'b> =
  fun input ->
    let a = ma input
    match a.produce with
    | None ->
      { produce  = None
        expected = a.expected
        output   = a.output
      }

    | Some va ->
      let b = k va (a.output |> Option.defaultValue input)
      if Option.isSome b.output
      then   b
      else
        { b with
            expected = a.expected + b.expected  // leak in errors of a
            output   = a.output                 // add consumption of a
        }

(*
  Run ma. If ma fails and not consumes, run b.
*)
let choose (ma : parser<'tok, 'a>) (mb : parser<'tok, 'a>) : parser<'tok, 'a> =
  fun input ->
    let a = ma input
    match a.produce, a.output with
    | None, None ->
      let b = mb input
      if Option.isSome b.output
      then   b
      else { b with expected = a.expected + b.expected }
    | _ -> a

(*
  Tinker with results of the parser.
*)
let manip (ma : parser<'tok, 'a>) (m : tokens<'tok> -> result<'tok, 'a> -> result<'tok, 'b>) : parser<'tok, 'b> =
  fun input ->
    m input (ma input)

(*
  Undo consumpton if parser have failed.
*)
let rollback (ma : parser<'tok, 'a>) : parser<'tok, 'a> =
  manip ma <| fun input res ->
    match res.produce with
    | Some _ ->   res
    | None   -> { res with output = None }

(*
  Check tha parser fails, undo any consumption.
*)
let notFollowedBy (ma : parser<'tok, 'a>) : parser<'tok, unit> =
  manip ma <| fun input res ->
    { produce  =
        match res.produce with
        | Some _ -> None
        | None   -> Some ()
      output   = None
      expected = Set []
    }

(*
  If parser ma fails and not consumes, change error message to single "msg".
*)
let deco (msg : string) (ma : parser<'tok, 'a>) : parser<'tok, 'a> =
  manip ma <| fun _ res ->
    match res.produce, res.output with
    | None, None -> { res with expected = Set [msg] }
    | other      ->   res

(*
  Monad/Alternative instance for parser.
*)
type ParserMonad() =
  member it.Bind(ma, amb)   = bind ma amb
  member it.Return(x)       = just x
  member it.ReturnFrom(ma)  = ma
  member it.Zero()          = empty
  member it.Combine(ma, mb) = choose ma mb
  member it.Delay(th)       = th()

let parser = new ParserMonad()

(*
  Change the output type of the parser.
*)
let map (f : 'a -> 'b) (ma : parser<'tok, 'a>) : parser<'tok, 'b> =
  parser {
    let! a = ma
    return f a
  }

(*
  If ma fails, return a instead.
*)
let option (a : 'a) (ma : parser<'tok, 'a>) : parser<'tok, 'a> =
  parser {
    return! ma
    return  a
  }

(*
  Sequence two independent parsers.
*)
let ap
  (mf : parser<'tok, 'a -> 'b>)
  (mx : parser<'tok, 'a>)
      : parser<'tok, 'b>
    =
  parser {
    let! f = mf
    let! x = mx
    return f x
  }

(*
  If ma fails, return None. Otherwise, wrap into Some.
*)
let optional (ma : parser<'tok, 'a>) : parser<'tok, option<'a>> =
  parser {
    return! map Some ma
    return  None
  }

(*
  Parse 0+ repeats of ma.
*)
let rec many (ma : parser<'tok, 'a>) : parser<'tok, List<'a>> =
  parser {
    return! some ma
    return  []
  }

(*
  Parse 1+ repeats of ma.
*)
and some (ma : parser<'tok, 'a>) : parser<'tok, List<'a>> =
  parser {
    let! x  = ma
    let! xs = many ma
    return (x :: xs)
  }

(*
  Return either current or starting or trailing position.
*)
let pos (toks : Option<tokens<'a>>) (input : input) : input =
  match toks with
  | Some ((pos, _) :: _) -> pos
  | Some []              -> Input.endOf input
  | None                 -> input
