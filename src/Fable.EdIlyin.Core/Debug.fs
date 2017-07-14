module Fable.EdIlyin.Core.Debug


let log tag x =
    do printfn "%s: %A" tag x
    x
