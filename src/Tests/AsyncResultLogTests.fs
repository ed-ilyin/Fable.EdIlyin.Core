module AsyncResultLog

open Fable.Core
open Fable.Core.JsInterop
open Fable.EdIlyin.Core
open Fable.EdIlyin.Core.Http
open Fable.Import

module JD = Fable.EdIlyin.Core.Json.Decode
module JE = Fable.EdIlyin.Core.Json.Encode


let equal (expected: 'T) (actual: 'T): unit =
    let assert' = importAll<obj> "assert"
    assert'?deepStrictEqual(actual, expected) |> ignore


[<Global>]
let it (msg: string) (f: unit->JS.Promise<'T>): unit = jsNative


it "asyncResultLog: log on bind" <| fun () ->
    async
        {   let! x =
                asyncResultLog
                    {
                        return
                    }
                "x tag",
                AsyncResultLog.singleton "abba" 42

            return equal 42 x
        }
        |> Async.StartAsPromise
