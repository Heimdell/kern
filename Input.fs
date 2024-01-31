
(*
  A string, equipped with position tracking.
*)
module Input

open Maybe

[<StructuredFormatDisplay("{line}:{column}")>]
type input =
  { filename : string
    source   : string
    offset   : int     // position in source, from 0
    line     : int     // from 1
    column   : int     // from 1
  }

(*
  Get next char, if possible.

  Returns a char and the stream, advanced one position forward.
*)
let uncons (input : input) : option<char * input> = opt {
  if input.source.Length > input.offset
  then
    let c = input.source[input.offset]
    return (c,
      if c = '\n'
      then
        { input with
            line   = input.line + 1
            offset = input.offset + 1
            column = 1
        }
      else
        { input with
            offset = input.offset + 1
            column = input.column + 1
        })
}

(*
  Peek a string between 2 positions.
*)
let span (start : input) (finish : input) : string =
  start.source.[start.offset.. finish.offset - 1]

(*
  Make input out of string.
*)
let ofString (source : string) : input =
  { filename = "<stdin>"
    source   = source
    offset   = 0
    line     = 1
    column   = 1
  }

(*
  Make input out of file's contents.
*)
let ofFile (filename : string) : input =
  let source = System.IO.File.ReadAllText(filename)
  { filename = filename
    source   = source
    offset   = 0
    line     = 1
    column   = 1
  }

(*
  Pick a line from the source that surrounds current position.
*)
let line (input : input) : string =
  let before : string = input.source.[0.. input.offset - 1]
  let after  : string = input.source.[input.offset..]
  (before
    |> Seq.rev
    |> Seq.takeWhile ((<>) '\n')
    |> Seq.rev
    |> System.String.Concat)
  +
  (after
    |> Seq.takeWhile ((<>) '\n')
    |> System.String.Concat)

(*
  Generate error report showing location, line and where in the line
  the error has happened.
*)
let report (input : input) : string =
  let prefix =
    input.filename
      + ":" + input.line.ToString()
      + ":" + input.column.ToString()
      + "> "
  let src    = line input
  let pos    = String.replicate (input.column - 1) " " + "^"
  prefix + "\n" + prefix + src + "\n" + prefix + pos + "\n" + prefix

(*
  Check that input has ended.
*)
let eos (input : input) : bool = input.source.Length = input.offset

(*
  Advance till the end of the input is reached.
*)
let rec endOf (input : input) : input =
  match uncons input with
  | None           -> input
  | Some (_, rest) -> endOf rest