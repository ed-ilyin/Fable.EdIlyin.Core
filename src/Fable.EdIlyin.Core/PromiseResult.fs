namespace Fable.EdIlyin.Core

open Fable.Import
open Fable.PowerPack


module PromiseResult =
    let andThen func promiseResult =
        promise
            {   let! result = promiseResult

                let! newAsyncResult =
                    match result with
                        | Error x -> promise { return Error x }
                        | Ok x -> func x

                return newAsyncResult
            }


    let result value = promise { return Ok value }


    type Builder () =
        member this.Bind (m, f) = andThen f m
        member this.Return m = result m


    let mapError func promiseResult =
        promise
            {   let! result = promiseResult

                return
                    match result with
                        | Ok value -> Ok value
                        | Error error -> func error |> Error
            }


[<AutoOpen>]
module PromiseResultAutoOpen =
    let promiseResult = new PromiseResult.Builder ()
