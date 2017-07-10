module Fable.EdIlyin.Core.Http.Fetch

open Fable.Core
open Fable.EdIlyin.Core
open Fable.EdIlyin.Core.Async
open Fable.PowerPack
open Fable.PowerPack.Fetch


[<Import("polyfill","es6-promise")>]
let promisePolyfill : unit -> unit = jsNative


do promisePolyfill ()


do JsInterop.importSideEffects "isomorphic-fetch"


let inline json decoder =
    Decode.primitive "an JSON"
        (fun (response: Response) ->
            async {
                let! json = response.json () |> Async.AwaitPromise
                let result = Json.Decode.decodeValue decoder json
                return result
            }
                |> Decode.Decoded
        )


let fetch url properties decoder =
    asyncResult {
        let! response =
            Fetch.tryFetch url properties
                |> Async.AwaitPromise
                |>> Result.mapError string

        let! decodedResponse =
            Decode.decode decoder response
                |> AsyncResult.fromResultAsync
                |>> Result.fromResultResult

        return decodedResponse
    }


let get url headers decoder =
    let properties =
        [   RequestProperties.Method HttpMethod.GET
            Fetch.requestHeaders headers
        ]

    fetch url properties decoder


let postText url headers text decoder =
    let properties =
        [   RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders headers
            U3.Case3 text |> RequestProperties.Body
        ]

    fetch url properties decoder


let postJson url headers pojo decoder =
    let defaultHeaders =
        List.append [ ContentType "application/json" ] headers

    let properties =
        [   RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders defaultHeaders
            Json.Encode.encode pojo
                |> U3.Case3
                |> RequestProperties.Body
        ]

    fetch url properties decoder
