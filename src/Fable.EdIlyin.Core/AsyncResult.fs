namespace Fable.EdIlyin.Core

open Fable.EdIlyin.Core.Async


module AsyncResult =
    let bind func asyncResult =
        async {
            let! result = asyncResult

            let! newAsyncResult =
                match result with
                    | Error x -> async { return Error x }
                    | Ok x -> func x

            return newAsyncResult
        }


    let succeed a = async { return Ok a }


    type AsyncResultBuilder () =
        member x.Bind(pr, f) = bind f pr
        member x.Return(a) = succeed a


    let fromResultAsync resultAsync =
        match resultAsync with
            | Error error -> Error error |> async.Return
            | Ok asyn -> asyn |>> Ok


[<AutoOpen>]
module AsyncResultImpl =
    let asyncResult = AsyncResult.AsyncResultBuilder ()
