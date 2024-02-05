
open Lexer
open Parser
open Raw

(*
  Echo-repl.
*)
let rec echo () =
  System.Console.Write "> "
  let line = System.Console.ReadLine()

  ignore <| testParser acted
    (Input.ofString line)
    (printfn "%s" << ppValue)

  System.Console.WriteLine ""

  echo ()

printfn "Kern language REPL\n"

echo ()