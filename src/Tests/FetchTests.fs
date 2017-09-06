module FetchTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.EdIlyin.Core
open Fable.EdIlyin.Core.Http
open Fable.Import
open Fable.PowerPack


module JD = Fable.EdIlyin.Core.Json.Decode


module JE = Fable.EdIlyin.Core.Json.Encode


let equal (expected: 'T) (actual: 'T): unit =
    let assert' = importAll<obj> "assert"
    assert'?deepStrictEqual(actual, expected) |> ignore


[<Global>]
let it (msg: string) (f: unit->JS.Promise<'T>): unit = jsNative


it "fetch: json echo" <| fun () ->
    promise
        {   let! x =
                Fetch.json Json.Decode.value
                    |> Fetch.get "http://echo.jsontest.com/abba/babba"
                        []

            let result =
                equal
                    (Json.Encode.object
                        ["abba", Json.Encode.string "babba"]
                        |> Ok
                    )
                    x

            return result
        }


it "fetch: wrong address" <| fun () ->
    promise
        {   let! x =
                Fetch.get "http://echoa.jsontest.com" [] Fetch.text

            let result =
                equal
                    // ("{\"name\":\"FetchError\",\"message\":\"request to http://echoa.jsontest.com failed, reason: getaddrinfo ENOTFOUND echoa.jsontest.com echoa.jsontest.com:80\",\"type\":\"system\",\"errno\":\"ENOTFOUND\",\"code\":\"ENOTFOUND\"}"
                    ("request to http://echoa.jsontest.com failed, reason: getaddrinfo ENOTFOUND echoa.jsontest.com echoa.jsontest.com:80"
                        |> Error
                    )
                    x

            return result
        }


it "fetch: json echo with decoder" <| fun () ->
    promise
        {   let! x =
                JD.field "abba" JD.string
                    |> Fetch.json
                    |> Fetch.get "http://echo.jsontest.com/abba/babba"
                        []

            let result = equal (Ok "babba") x
            return result
        }


it "fetch: json echo with decoder: error" <| fun () ->
    promise
        {   let! x =
                JD.field "abbax" JD.string
                    |> Fetch.json
                    |> Fetch.get "http://echo.jsontest.com/abba/babba"
                        []

            let result =
                equal
                    (@"Expecting a String field 'abbax', but instead got: ""{\""abba\"":\""babba\""}"""
                        |> Error
                    )
                    x

            return result
        }


// it "fetch: cookie" <| fun () ->
//     async
//         {   let! x =
//                 Fetch.fetch
//                     "http://www.whatarecookies.com/cookietest.asp"
//                     []
//                     Fetch.text

//             let result =
//                 equal (Ok "abbabbababa") x

//             return x
//         }
//         |> Async.StartAsPromise
