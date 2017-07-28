module ResultTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.EdIlyin.Core


let inline equal (expected: 'T) (actual: 'T): unit =
    let assert' = importAll<obj> "assert"
    assert'?deepStrictEqual(actual, expected) |> ignore


[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative


it "result: computation expression: return" <| fun () ->
    result
        {   let! r = Ok 42
            return r
        }
        |> equal (Ok 42)


it "result: computation expression: return from" <| fun () ->
    result
        {   let r = Ok 42
            return! r
        }
        |> equal (Ok 42)


it "result: computation expression: zero" <| fun () ->
    result { do sprintf "%i" 42 |> ignore } |> equal (Ok ())
