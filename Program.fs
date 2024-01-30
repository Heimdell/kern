
open Lexer
open Parser

// testParser value "(define [singleton k v . kvs] {:k k :v v"

let is_a a = a = 'a'
let is_b a = a = 'b'

Input.ofFile "example.lisp"
  |> tokens
  |> printfn "%O"
