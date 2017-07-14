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


let execute func (channel: AsyncReplyChannel<_>) model =
    do func () |> channel.Reply

    Debug.log "executed"
        { model with
            queue = Queue.push model.queue DateTime.Now.Millisecond
        }


let fetch model func channel =
    match Queue.length model.queue with
        | l when l < model.quantity -> async.Return model

        | _ ->
            match Queue.pull model.queue with
                | None -> async.Return model

                | Some (was, tail) ->
                    DateTime.Now.Millisecond
                        - was
                        |> (-) model.millisecond
                        |> Async.Sleep
                        |>> fun _ -> { model with queue = tail }

        |>> execute func channel


let body model (agent: MailboxProcessor<_>) =
    let rec loop state =
        agent.Receive ()
            >>= function
                | Die -> async.Zero ()

                | Fetch (func, channel) ->
                    fetch state func channel >>= loop

    loop model


let start quantity millisecond =
    let agent =
        body
            {   quantity = quantity
                millisecond = millisecond
                queue = Queue.empty
            }
            |> MailboxProcessor.Start

    let innerFn func =
        agent.PostAndAsyncReply (fun reply -> Fetch (func, reply))

    innerFn
