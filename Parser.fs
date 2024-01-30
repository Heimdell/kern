
module rec Parser

open Maybe
open Lexer
open Input

type either<'a, 'b> =
  | Ok  of 'a
  | Err of 'b

let either (f : 'a -> 'c) (g : 'b -> 'c) : either<'a, 'b> -> 'c =
  function
  | Ok  a -> f a
  | Err b -> g b

let bimap (f : 'a -> 'c) (g : 'b -> 'd) : either<'a, 'b> -> either<'c, 'd> =
  either (Ok << f) (Err << g)

let bimap2 (f : 'a -> 'c) (g : 'b -> 'd) : 'a * 'b -> 'c * 'd =
  fun (a, b) -> f a, g b

type tokens = list<input * token>
type error  = Set<string>
type result<'a> =
  { output   : option<tokens>
    produce  : either<error * 'a, error>
  }

type parser<'a> = tokens -> result<'a>

let just (x : 'a) : parser<'a> =
  fun input ->
    { produce  = Ok (Set [] , x)
      output   = None
    }

let empty : parser<'a> =
  fun input ->
    { produce  = Err (Set [])
      output   = None
    }

let satisfy (pred : token -> bool) : parser<input * token> =
  function
  | [] ->
    { produce  = Err (Set [])
      output   = None
    }
  | ((pos, tok) :: rest) as input ->
    if pred tok
    then
      { produce  = Ok (Set [], (pos, tok))
        output   = Some rest
      }
    else
      { produce  = Err (Set [])
        output   = None
      }

// let bind (ma : parser<'a>) (k : 'a -> parser<'b>) : parser<'b> =
  // fun input ->
  //   match ma input with
  //   | { produce = Err errs_a
  //       output  = None
  //     } ->
  //     match mb input with
  //     | { produce = Err errs_b
  //         output  = output_b
  //       } ->
  //       {

  //       }

// let choose (ma : parser<'a>) (mb : parser<'a>) : parser<'a> =
//   fun input ->
//     let a = ma input
//     match a.produce, a.consumed with
//     | Err warns, false ->
//       let b = mb input
//       match b.produce, b.consumed with
//       | Err errs, false ->
//         { produce  = Err (warns + errs)
//           consumed = false
//           output   = input
//         }
//       | Ok (warns_b, vb), _ ->
//         { produce  = Ok (warns + warns_b, vb)
//           consumed = b.consumed
//           output   = b.output
//         }
//       | _ -> b
//     | _ -> a

// let select (ms : List<parser<'a>>) = List.fold choose empty ms

// let option (a : 'a) (ma : parser<'a>) : parser<'a> =
//   select
//     [ ma
//       just a
//     ]

// let manip (ma : parser<'a>) (m : tokens -> result<'a> -> result<'b>) : parser<'b> =
//   fun input ->
//     m input (ma input)

// let rollback (ma : parser<'a>) : parser<'a> =
//   manip ma <| fun input res ->
//     match res.produce with
//     | Ok  x ->   res
//     | Err _ ->
//       { res with
//           consumed = false
//           output   = input
//       }

// let notFollowedBy (ma : parser<'a>) : parser<unit> =
//   manip ma <| fun input res ->
//     { produce =
//         match res.produce with
//         | Ok  x -> Err (Set [])
//         | Err _ -> Ok  (Set [], ())
//       output   = input
//       consumed = false
//     }

// let map (f : 'a -> 'b) (ma : parser<'a>) : parser<'b> =
//   manip ma <| fun input res ->
//     { produce  = bimap (bimap2 id f) id res.produce
//       consumed = res.consumed
//       output   = res.output
//     }

// let optional (ma : parser<'a>) : parser<option<'a>> =
//   select
//     [ map (fun a -> Some a) ma
//       just None
//     ]

// // // let ap
// // //   (mf : parser<'a -> 'b>)
// // //   (mx : parser<'a>)
// // //       : parser<'b>
// // //     =
// // //   bind mf <| fun f -> map f mx

// // // let deco (msg : string) (ma : parser<'a>) : parser<'a> =
// // //   manip ma <| fun _ res ->
// // //     match res with
// // //     | {result = Err _; output = 0} -> {result = Err [msg]; output = 0}
// // //     | other                        -> other

// // // type ParserMonad() =
// // //   member it.Bind(ma, amb)  = bind ma amb
// // //   member it.Return(x)      = just x
// // //   // member it.ReturnFrom(ma) = ma

// // // let parser = new ParserMonad()

// // // let rec many (ma : parser<'a>) : parser<List<'a>> =
// // //   select
// // //     [ some ma
// // //       just []
// // //     ]

// // // and some (ma : parser<'a>) : parser<List<'a>> =
// // //   parser {
// // //     let! x  = ma
// // //     let! xs = many ma
// // //     return (x :: xs)
// // //   }

// // // let guard (b : bool) : parser<unit> =
// // //   if b then just () else empty

// // // let nameChar : parser<char> = satisfy <| fun c ->
// // //      c >= 'a' && c <= 'z'
// // //   || c = '-'
// // //   || c = '!'
// // //   || c = '?'

// // // let name : parser<string> = trace (some nameChar) |> deco "name"

// // // let digit : parser<char> = satisfy <| fun c -> c >= '0' && c <= '9'

// // // let char (c : char) : parser<char> = satisfy <| (=) c

// // // let rec traverse (f : 'a -> parser<'b>) (xs : List<'a>) : parser<List<'b>> =
// // //   match xs with
// // //   | [] -> just []
// // //   | x :: xs -> parser {
// // //     let! x  =          f x
// // //     let! xs = traverse f xs
// // //     return x :: xs
// // //   }

// // // let str (s : string) : parser<string> =
// // //   traverse char (Seq.toList s)
// // //     |> trace
// // //     |> deco s

// // // let number : parser<either<int, float>> =
// // //   parser {
// // //     let! sign  = option "" <| str "-"
// // //     let! start = trace (some digit)
// // //     let! rest = optional <| trace (parser {
// // //       let! dot = str "."
// // //       let! fin = trace (some digit)
// // //       return dot + fin
// // //     })
// // //     return
// // //       match rest with
// // //       | None      -> Ok  (int   (sign + start))
// // //       | Some rest -> Err (float (sign + start + rest))
// // //   }
// // //   |> deco "number"

// // // let charLit =
// // //   select
// // //     [ parser
// // //         { let! () = notFollowedBy <| select [str "\\"; str "\""]
// // //           let! c  = satisfy <| fun _ -> true
// // //           return c
// // //         }

// // //       parser
// // //         { let! _ = str "\\"

// // //           let! c =
// // //             select
// // //               [ map (fun _ -> '\\') (str "\\")
// // //                 map (fun _ -> '\n') (str "n")
// // //                 map (fun _ -> '\"') (str "\"")
// // //               ]
// // //           return c
// // //         }
// // //     ]

// // // let stringLit = trace (many charLit)

// // // let charLiteral =
// // //   parser {
// // //     let! _ = char '\''
// // //     let! c = charLit
// // //     let! _ = char '\''
// // //     return c
// // //   } |> deco "char literal"

// // // let stringLiteral =
// // //   parser {
// // //     let! _ = char '\"'
// // //     let! c = stringLit
// // //     let! _ = char '\"'
// // //     return c
// // //   } |> deco "string literal"

// // // let token (p : parser<'a>) : parser<'a> =
// // //   parser {
// // //     let! a = p
// // //     let! _ = many <| satisfy (fun c -> System.Char.IsWhiteSpace(c))
// // //     return a
// // //   }

// // // let report (i : input) a : string =
// // //   let before =
// // //     i.text[0.. i.shift - 1]
// // //       |> Seq.rev
// // //       |> Seq.toList
// // //       |> List.takeWhile ((<>) '\n')
// // //       |> List.rev
// // //       |> List.map string
// // //       |> String.concat ""

// // //   let after =
// // //     i.text[i.shift..]
// // //       |> Seq.toList
// // //       |> List.takeWhile ((<>) '\n')
// // //       |> List.rev
// // //       |> List.map string
// // //       |> String.concat ""

// // //   before + after + "\n" + String.replicate before.Length " " + "^" + "\n" + a.ToString()

// // // let testParser (ma : parser<'a>) (i : string) : unit =
// // //   match ma.parse {text = i; shift = 0} with
// // //   | {result = Ok  a}             -> printfn "%O"    a
// // //   | {result = Err a; output = j} -> printfn "%s" <| report {text = i; shift = j} a

// // // type list =
// // //   { elems : List<value>
// // //     trail : option<value>
// // //   }

// // // and value =
// // //   | Atom  of string
// // //   | Var   of string
// // //   | Int   of int
// // //   | Float of float
// // //   | Bool  of bool
// // //   | Str   of string
// // //   | Char  of char
// // //   | List  of list

// // // let atom : parser<value> = parser {
// // //   let! _ = str ":"
// // //   let! a = name
// // //   return Atom a
// // // }

// // // let var : parser<value> = map Var name

// // // let numLit : parser<value> = map (either Int Float) number

// // // let boolLit : parser<value> =
// // //   parser {
// // //     let! _ = str "#"
// // //     let! b =
// // //       select
// // //         [ map (fun _ -> true) (str "t")
// // //           map (fun _ -> true) (str "f")
// // //         ]
// // //     return Bool b
// // //   } |> deco "boolean literal"

// // // let strVal  : parser<value> = map Str  stringLiteral
// // // let charVal : parser<value> = map Char charLiteral

// // // let rec value : parser<value> =
// // //   token <| select
// // //     [ atom
// // //       var
// // //       numLit
// // //       boolLit
// // //       strVal
// // //       charVal
// // //       map List listp
// // //     ]

// // // and listp : parser<list> = parser {
// // //   let! close =
// // //     token <| select
// // //       [ map (fun _ -> "]") (str "[")
// // //         map (fun _ -> "}") (str "{")
// // //         map (fun _ -> ")") (str "(")
// // //       ]
// // //   let! elems = some value
// // //   let! trail =
// // //     optional <| parser {
// // //       let! _ = token (str ".")
// // //       let! e = value
// // //       return e
// // //     }
// // //   let! _ = token (str close)
// // //   return
// // //     { elems = elems
// // //       trail = trail
// // //     }
// // // }