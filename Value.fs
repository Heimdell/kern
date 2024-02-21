
module Value

open Raw
open Parsing

type run =
  | RQuoted of value
  | RList   of List<run>
  | RNum    of float
  | RChar   of char
  | RStr    of string
  | RBool   of bool
  | SF      of runner
  | BIF     of functor

and runner  =
  | Run of (value -> runnerF<run>)

and runnerF<'a> =
  Map<string, runner>
    -> Map<string, run>
    -> either<'a * Map<string, runner> * Map<string, run>, string>

and functor = run -> either<run, string>

type LispMonad () =
  member it.Return(a) = fun sfs env -> Ok(a, sfs, env)
  member it.ReturnFrom(ma) = ma
  member it.Bind
    ( ma  :       runnerF<'a>
    , amb : 'a -> runnerF<'b>
    )     :       runnerF<'b> = fun sfs env ->
      match ma sfs env with
      | Err e        -> Err e
      | Ok (a, sfs', env') -> amb a sfs' env'

let lisp = new LispMonad()

let rec ppRun : run -> string =
  function
  | RQuoted v  -> "'" + ppValue v
  | RList   xs -> "(" + String.concat " " (List.map ppRun xs) + ")"
  | RNum    n  -> n.ToString()
  | RChar   c  -> "#\"" + c.ToString() + "\""
  | RStr    s  -> "\"" + s.ToString() + "\""
  | RBool   b  -> "#" + if b then "true" else "false"
  | SF      sf -> "<SF>"
  | BIF     f  -> "<FUN>"

let die (func : string) (msg : string) (xs : run) : string =
  func + ": expected " + msg + ", not " + ppRun xs

let fail (msg : string) : runnerF<'a> =
  fun _ _ -> Err msg

let specialForms : runnerF<Map<string, runner>> =
  fun sfs env -> Ok (sfs, sfs, env)

let environment : runnerF<Map<string, run>> =
  fun sfs env -> Ok (env, sfs, env)

let rec run (value : value) : runnerF<run> =
  lisp {
    match value with
    | VAtom (VNum  n) -> return RNum n
    | VAtom (VStr  n) -> return RStr  n
    | VAtom (VBool n) -> return RBool n
    | VAtom (VChar n) -> return RChar n
    | VAtom (VName n) ->
      let! sfs = specialForms
      match Map.tryFind n sfs with
      | Some sf -> return SF sf
      | None ->
        let! env = environment
        match Map.tryFind n env with
        | Some value -> return value
        | None -> return! fail ("symbol " + n + " is not defined")

    | VCons pair ->
      match! run pair.car with
      | SF (Run runner) -> return! runner pair.cdr
      | BIF lambda ->
        let! list = evalList pair.cdr
        match lambda (RList list) with
        | Ok a -> return a
        | Err e -> return! fail e

      | other ->
        match pair.cdr with
        | VNil -> return other
        | rest -> return! run rest

    | VNil -> return RList []
  }

and evalList (value : value) : runnerF<List<run>> =
  lisp {
    match value with
    | VCons pair ->
      let! a  = run      pair.car
      let! az = evalList pair.cdr
      return (a :: az)
    | VNil -> return []
    | other -> return! fail ("not a list in funcall " + ppValue other)
  }

exception TypeError of string * run

let ppTypeError : exn -> string =
  function
  | TypeError (ty, value) ->
    "expected type " + ty + ", got value " + ppRun value
  | _ -> "?"

type interop<'a> =
  { type_ :  string
    forth : 'a   ->  run
    back  :  run -> 'a
  }

let aFloat : interop<float> =
  { type_ = "float"
    forth = RNum
    back  =
      function
      | RNum n -> n
      | x      -> raise <| TypeError ("float", x)
  }

let aBool : interop<bool> =
  { type_ = "bool"
    forth = RBool
    back  =
      function
      | RBool n -> n
      | x      -> raise <| TypeError ("bool", x)
  }

let aChar : interop<char> =
  { type_ = "bool"
    forth = RChar
    back  =
      function
      | RChar n -> n
      | x      -> raise <| TypeError ("char", x)
  }

let aRun : interop<run> =
  { type_ = "run"
    forth = id
    back  = id
  }

let aListOf (elem : interop<'a>) : interop<List<'a>> =
  { type_ = "list<" + elem.type_ + ">"
    forth = RList << List.map elem.forth
    back  =
      function
      | RList xs -> List.map elem.back xs
      | x        -> raise <| TypeError ("list<" + elem.type_ + ">", x)
  }

let aString : interop<string> =
  { type_ = "string"
    forth = RStr
    back  =
      function
      | RStr s -> s
      | x      -> raise <| TypeError ("string", x)
  }

let aValue : interop<value> =
  { type_ = "quoted"
    forth = RQuoted
    back  =
      function
      | RQuoted s -> s
      | x         -> raise <| TypeError ("quoted", x)
  }

open Maybe

let (.&&.)
  (first  : interop<'a>)
  (second : interop<'b>)
          : interop<'a * 'b> =
  { type_ = "(" + first.type_ + ", " + second.type_ + ")"
    forth = fun (a, b) ->
      match second.forth b with
      | RList xs -> RList (first.forth a :: xs)
      | b        -> RList [first.forth a; b]

    back =
      function
      | RList (a :: b) ->
        let a = first.back a
        let b = second.back (RList b)
        a, b
      | x -> raise <| TypeError ("pair", x)
  }

let (.||.)
  (first  : interop<'a>)
  (second : interop<'b>)
          : interop<either<'a, 'b>> =
  { type_ = "either<" + first.type_ + ", " + second.type_ + ">"
    forth =
      function
      | Ok a -> first.forth a
      | Err b -> second.forth b

    back = fun run ->
      try
        Ok <| first.back run
      with e ->
        Err <| second.back run
  }

let aUnit : interop<unit> =
  { type_ = "unit"
    forth = fun () -> RList []
    back =
      function
      | RList [] -> ()
      | x        -> raise <| TypeError ("unit", x)
  }

let first (f : 'a -> 'b) (e : either<'a, 'c>) : either<'b, 'c> =
  match e with
  | Ok a -> Ok (f a)
  | Err a -> Err a

let (.->.)
  (domain : interop<'a>)
  (image  : interop<'b>)
          : interop<'a -> either<'b, string>> =
  { type_ = domain.type_ + " -> " + image.type_
    forth = fun f ->
      BIF (domain.back >> f >> first image.forth)

    back =
      function
      | BIF run -> domain.forth >> run >> first image.back
      | x       -> raise <| TypeError (domain.type_ + " -> " + image.type_, x)
  }

let decl
  (name  : string)
  (ty    : interop<'a>)
  (value : 'a)
         : string * run =
  name , ty.forth value

let listF  : string * run =
  decl "list" (aListOf aRun .->. aRun) <| fun args ->
    Ok (RList args)

let quoteR : runner  =
  Run <| fun value ->
    lisp {
      match value with
      | VCons {car = value; cdr = VNil} -> return RQuoted value
      | other                           -> return! fail (die "quote" "(any)" (RQuoted other))
    }

let consF  : string * run =
  decl "cons" ((aRun .&&. (aRun .&&. aUnit)) .->. aRun) <|
    fun (a, (xs, ())) ->
      match xs with
      | RList xs -> Ok (RList (a :: xs))
      | xs       -> Err (die "cons" "(any list)" xs)

let carF : string * run =
  decl "car" (((aRun .&&. aRun) .&&. aUnit) .->. aRun) <|
    fun ((a, b), ()) -> Ok a

let cdrF : string * run =
  decl "cdr" (((aRun .&&. aRun) .&&. aUnit) .->. aRun) <|
    fun ((a, b), ()) -> Ok b

let typeofF : string * run =
  decl "typeof" ((aRun .&&. aUnit) .->. aRun) <|
    fun (value, ()) ->
      match value with
      | RQuoted v -> Ok (RStr "quote")
      | RList   v -> Ok (RStr "list")
      | RNum    n -> Ok (RStr "num")
      | RChar   c -> Ok (RStr "char")
      | RStr    c -> Ok (RStr "string")
      | RBool   c -> Ok (RStr "bool")
      | SF      c -> Ok (RStr "SF")
      | BIF     c -> Ok (RStr "function")

let rec parseCondArgs : value -> runnerF<list<bool * value>> =
  fun value ->
    lisp {
      match value with
      | VCons {car = yes; cdr = VCons {car = body; cdr = rest}} ->
        let! yes  = run yes
        let  bYes = aBool.back yes
        let! rest = parseCondArgs rest
        return (bYes, body) :: rest
      | VNil ->
        return []
      | x -> return! fail (die "cond" "even number of args" (RQuoted x))
    }

exception NoTrueInCond

let rec condLookup : list<bool * value> -> value =
  function
  | (true, body) :: _    -> body
  | _            :: rest -> condLookup rest
  | []                   -> raise NoTrueInCond

let condR : runner =
  Run <|
    fun value ->
      lisp {
        let! list = parseCondArgs value
        let  body = condLookup list
        return! run body
      }

let binaryArith = aListOf aFloat .->. aFloat
let binaryArith1 = (aFloat .&&. aListOf aFloat) .->. aFloat

let plusF  = decl "+"   binaryArith  <| fun az -> Ok (List.fold (+) 0 az)
let minusF = decl "-"   binaryArith  <| fun az -> Ok (List.fold (-) 0 az)
let multF  = decl "*"   binaryArith  <| fun az -> Ok (List.fold (*) 1 az)
let divF   = decl "/"   binaryArith1 <| fun (a, az) -> Ok (List.fold (/) a az)
let modF   = decl "mod" binaryArith1 <| fun (a, az) -> Ok (List.fold (%) a az)

let appendF : string * run =
  decl "++"
    ((    (aListOf aRun  .&&. (aListOf aRun  .&&. aUnit))
     .||. (aString .&&. (aString .&&. aUnit))
     ) .->. (aListOf aRun .||. aString))
    <|
    function
    | Ok  (a, (b, ())) -> Ok (Ok  (a @ b))
    | Err (a, (b, ())) -> Ok (Err (a + b))

let rec pairwise (p : 'a -> 'a -> bool) : list<'a> -> bool =
  function
  | a :: b :: az -> p a b && pairwise p (b :: az)
  | other        -> true


let binaryOrder =
    (    (     aListOf aFloat
         .||. (aListOf aString
         .||. (aListOf aChar
         .||.  aListOf aBool
         )))
    .->. aBool
    )

let lessF =
  decl "<" binaryOrder <|
    function
    |           Ok  az   -> Ok (pairwise (<) az)
    | Err      (Ok  az)  -> Ok (pairwise (<) az)
    | Err (Err (Ok  az)) -> Ok (pairwise (<) az)
    | Err (Err (Err az)) -> Ok (pairwise (<) az)

let leqF =
  decl "<=" binaryOrder <|
    function
    |           Ok  az   -> Ok (pairwise (<=) az)
    | Err      (Ok  az)  -> Ok (pairwise (<=) az)
    | Err (Err (Ok  az)) -> Ok (pairwise (<=) az)
    | Err (Err (Err az)) -> Ok (pairwise (<=) az)

let greaterF =
  decl ">" binaryOrder <|
    function
    |           Ok  az   -> Ok (pairwise (>) az)
    | Err      (Ok  az)  -> Ok (pairwise (>) az)
    | Err (Err (Ok  az)) -> Ok (pairwise (>) az)
    | Err (Err (Err az)) -> Ok (pairwise (>) az)

let geqF =
  decl ">=" binaryOrder <|
    function
    |           Ok  az   -> Ok (pairwise (>=) az)
    | Err      (Ok  az)  -> Ok (pairwise (>=) az)
    | Err (Err (Ok  az)) -> Ok (pairwise (>=) az)
    | Err (Err (Err az)) -> Ok (pairwise (>=) az)

let equalValue (l : value) (r : value) : bool = false

let rec equalRun (l : run) (r : run) : bool =
  match l, r with
  | RQuoted l , RQuoted r -> equalValue l r
  | RList   l , RList   r ->
    try
      List.forall2 equalRun l r
    with _ ->
      false
  | RNum  l   , RNum  r   -> l = r
  | RChar l   , RChar r   -> l = r
  | RStr  l   , RStr  r   -> l = r
  | RBool l   , RBool r   -> l = r
  | SF    l   , SF    r   -> false
  | BIF   l   , BIF   r   -> false
  | _                     -> false

let equality = aListOf aRun .->. aBool

let equalF = decl "="  equality <| fun ab -> Ok      (pairwise equalRun ab)
let neqF   = decl "/=" equality <| fun ab -> Ok (not (pairwise equalRun ab))

let printF =
  decl "print"
    (aListOf
      ( (aFloat .||. aBool)
      .||.
        (aChar .||. aString)
      ) .->. aUnit)
    <| fun elems ->
      for elem in elems do
        match elem with
        | Ok (Ok  num)   -> printf "%O" num
        | Ok (Err true)  -> printf "true"
        | Ok (Err false) -> printf "false"
        | Err (Ok  c)    -> printf "%c" c
        | Err (Err c)    -> printf "%s" c
      printfn ""
      Ok ()

let specialsR : runner =
  Run <| fun value ->
    lisp {
      match value with
      | VNil ->
        let! sfs = specialForms
        return RList (List.map RStr (Seq.toList (Map.keys sfs)))

      | x ->
        return! fail (die "special-forms" "()" (RQuoted x))
    }

let declarationsR : runner =
  Run <| fun value ->
    lisp {
      match value with
      | VNil ->
        let! sfs = environment
        return RList (List.map RStr (Seq.toList (Map.keys sfs)))

      | x ->
        return! fail (die "declarations" "()" (RQuoted x))
    }

let evalF : runner =
  Run <| fun value ->
    lisp {
      match value with
      | VCons
        { car = VCons
            { car = VAtom (VName "quote")
              cdr = value
            }
          cdr = VNil
        }
          -> return! run value
      | VCons
        { car = other
          cdr = VNil
        }
          -> return! run other
      | other ->
        return! fail (die "eval" "(quoted | any)" (RQuoted other))
    }

let stdSFs : Map<string, runner> =
  Map.ofList
    [
       "quote", quoteR
       "cond",  condR
       "special-forms" , specialsR
       "declarations" , declarationsR
       "eval" , evalF
    ]

let stdEnv : Map<string, run> =
  Map.ofList
    [
      listF
      consF
      carF
      cdrF
      typeofF
      plusF
      minusF
      multF
      divF
      modF
      printF
      lessF
      leqF
      greaterF
      geqF
      equalF
      neqF
      appendF
    ]

// // quote typeof
// // cons car cdr cond print

// // + - * / %-mod < <= > >= ==-= !=-<>-/= ++