
module Input

open Maybe

[<StructuredFormatDisplay("{line}:{column}")>]
type input =
  { filename : string
    source   : string
    offset   : int
    line     : int
    column   : int
  }

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

let span (start : input) (finish : input) : string =
  start.source.[start.offset.. finish.offset - 1]

let ofString (source : string) : input =
  { filename = "<stdin>"
    source   = source
    offset   = 0
    line     = 1
    column   = 1
  }

let ofFile (filename : string) : input =
  let source = System.IO.File.ReadAllText(filename)
  { filename = filename
    source   = source
    offset   = 0
    line     = 1
    column   = 1
  }


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

let report (input : input) : string =
  let prefix =
    input.filename
      + ":" + input.line.ToString()
      + ":" + input.column.ToString()
      + "> "
  let src    = line input
  let pos    = String.replicate (input.column - 1) " " + "^"
  prefix + "\n" + prefix + src + "\n" + prefix + pos

let eos (input : input) : bool = input.source.Length = input.offset