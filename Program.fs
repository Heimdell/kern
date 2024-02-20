
open Lexer
open Parser
open Raw
open Value
open Parsing

(*
  Echo-repl.
*)
let rec echo sfs env =
  System.Console.Write "> "
  let line = System.Console.ReadLine()

  let res =
    testParser sequence
      (Input.ofString line)
      (fun seq ->
        let list = listToCons seq None
        try
          let res  = run list sfs env
          match res with
          | Err e ->
            printfn "Error: %s" e
            sfs, env

          | Ok (a, sfs', env') ->
            printfn "%s" (ppRun a)
            sfs', env'
        with te ->
          printfn "%s" (ppTypeError te)
          sfs, env
      )

  System.Console.WriteLine ""

  match res with
  | Some (sfs', env') -> echo sfs' env'
  | None              -> echo sfs  env

printfn "Kern language REPL\n"

echo stdSFs stdEnv