
module Maybe

type LexerMonad () =
  member it.Return    (a)  = Some a
  member it.ReturnFrom(ma) = ma

  member it.Bind(ma, k) =
    match ma with
    | Some a -> k a
    | None   -> None

  member it.Combine(ma, mb) =
    match ma with
    | Some a -> Some a
    | None   -> mb

  member it.Delay(ma) = ma ()
  member it.Zero ()   = None

let opt = new LexerMonad()
