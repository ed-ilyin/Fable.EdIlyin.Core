module Fable.EdIlyin.Core.Http.Fetch

open Fable.Core
open Fable.EdIlyin.Core
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.Import


[<Import("polyfill","es6-promise")>]
let promisePolyfill : unit -> unit = jsNative


do promisePolyfill ()


do JsInterop.importSideEffects "isomorphic-fetch"


let fetch url properties decoder =
    promise
        {   let! response =
                GlobalFetch.fetch
                    (RequestInfo.Url url, requestProps properties)

            let! result =
                Decode.decode decoder response
                    |> Result.unpack (Error >> promise.Return) id

            return result
        }
        |> Promise.result
        |> Promise.map
            (Result.mapError (fun (e: System.Exception) -> e.Message)
                >> Result.andThen id
            )


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


let text =
    Decode.primitive "a Text"
        (fun (response: Response) ->
            response.text ()
                |> Promise.result
                |> PromiseResult.mapError
                    (fun (e: System.Exception) -> e.Message)
                |> Ok
        )


let json decoder =
    Decode.primitive "an JSON"
        (fun (response: Response) ->
            promise
                {   let! json = response.json ()
                    let result = Json.Decode.decodeValue decoder json
                    return result
                }
                |> Ok
        )


let response: Decode.Decoder<Response,JS.Promise<Result<Response,string>>> =
    Decode.primitive "an HTTP response" (Ok >> promise.Return >> Ok)
