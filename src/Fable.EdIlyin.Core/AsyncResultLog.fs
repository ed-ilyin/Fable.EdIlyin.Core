namespace Fable.EdIlyin.Core


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


    let singleton x =
        let result = Ok x
        resultLog "singleton" result |> async.Return


    type ComputationExpression () =
        member this.Bind (m, f) = andThen f m
        member this.Return x = singleton x
        member this.ReturnFrom m = m

        member this.Zero () =
            Ok () |> resultLog "zero" |> async.Return


    let fromAsyncResult tag asyncResult =
        async {
            let! result = asyncResult
            let! response = resultLog tag result |> async.Return
            return response
        }

    let print asyncResultLog =
        async
            {   let! _, log = asyncResultLog
                do List.iter (printfn "%s: %s" |> uncurry) log
            }


[<AutoOpen>]
module AsyncResultLogAutoOpen =
    let asyncResultLog = AsyncResultLog.ComputationExpression ()
