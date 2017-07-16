module Fable.EdIlyin.Core.Throttle

open System
open Fable.EdIlyin.Core.Async


type Model =
    {   quantity: int
        millisecond: int
        queue: int Queue.queue
    }


and Msg<'a> =
    | Die
    | Fetch of (unit -> 'a) * AsyncReplyChannel<'a>


let nowMilliseconds () =
    let milliseconds = DateTime.Now.Ticks |> TimeSpan
    int milliseconds.TotalMilliseconds


let private execute func (channel: AsyncReplyChannel<_>) model =
    // do printfn "preexecuting... %A" model
    do func () |> channel.Reply

    {model with queue = nowMilliseconds () |> Queue.push model.queue}
        // |> Debug.log "executed"


let private fetch model func channel =
    match Queue.length model.queue with
        | l when l < model.quantity ->
            async.Return model

        | _ ->
            match Queue.pull model.queue with
                | None -> async.Return model

                | Some (was, tail) ->
                    nowMilliseconds ()
                        - was
                        |> (-) model.millisecond
                        |> Async.Sleep
                        |>> fun _ -> { model with queue = tail }

        |> Async.map (execute func channel)


let private body model (agent: MailboxProcessor<_>) =
    let rec loop state =
        agent.Receive ()
            >>= function
                | Die -> async.Zero ()

                | Fetch (func, channel) ->
                    fetch state func channel >>= loop

    loop model


let start quantity millisecond =
    body
        {   quantity = quantity
            millisecond = millisecond
            queue = Queue.empty
        }
        |> MailboxProcessor.Start


let add (throttler: MailboxProcessor<_>) func =
    throttler.PostAndAsyncReply (fun channel -> Fetch (func, channel))

