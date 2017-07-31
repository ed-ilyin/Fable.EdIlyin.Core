namespace Fable.EdIlyin.Core

open System
open Fable.Core


module AsyncResultLog =
    let log tag result = [ tag, sprintf "%A" result ]


    let andThen func asyncResultLog =
        async
            {   let! result, l = asyncResultLog

                let! response =
                    match result with
                        | Error message ->
                            (Error message, l) |> async.Return

                        | Ok x ->
                            async
                                {   let! newResult, newLog = func x

                                    let resultLog =
                                        newResult
                                            , l
                                            @ newLog
                                            @ log "and then" newResult

                                    return resultLog
                                }

                return response
            }


    let resultLog tag result = result, log tag result


    let singleton tag x =
        let result = Ok x
        resultLog tag result |> async.Return


    type ComputationExpression () =
        member this.Bind (m, f) = andThen f m
        member this.Return x = singleton "return" x
        member this.ReturnFrom m = m

        member this.Zero () =
            Ok () |> resultLog "zero" |> async.Return


    let fromAsyncResult tag asyncResult =
        async {
            let! result = asyncResult
            let! response = resultLog tag result |> async.Return
            return response
        }


    let print maxChars asyncResultLog =
        let truncate s =
            if String.length s <= maxChars
                then s
                else
                    Seq.truncate (maxChars - 3) s
                        |> String.Concat
                        |> flip (+) "..."

        async
            {   let! _, log = asyncResultLog
                do List.iter
                    (fun (tag, record) ->
                        truncate record |> printfn "%s: %s" tag
                    )
                    log
            }


    let fromPromise tag promise =
        async
            {   let! x = Async.AwaitPromise promise
                return! singleton tag x
            }


    let fromResultAsyncResult tag resultAsyncResult =
        async
            {   let! result =
                    match resultAsyncResult with
                        | Error message ->
                            Error message |> async.Return

                        | Ok asyncResult -> asyncResult

                return resultLog tag result
            }


    let fromPromiseResult tag promiseResult =
        async
            {   let! x = Async.AwaitPromise promiseResult
                return resultLog tag x
            }


    let mapError func asyncResultLog =
        async
            {
                let! result, log = asyncResultLog
                let response = Result.mapError func result, log
                return response
            }


    let fromResult tag result =
        async { return resultLog tag result }


    let catch asyncResultLog =
        async
            {   let! choice = asyncResultLog |> Async.Catch
                let result = Result.ofChoice choice

                let response =
                    match result with
                        | Error e ->
                            Error e.Message |> resultLog "catch"

                        | Ok (Error e, l) ->
                            Error e, l @ log "catch" (Error e)

                        | Ok (Ok x, l) -> Ok x, l @ log "catch" (Ok x)

                return response
            }


[<AutoOpen>]
module AsyncResultLogAutoOpen =
    let asyncResultLog = AsyncResultLog.ComputationExpression ()
