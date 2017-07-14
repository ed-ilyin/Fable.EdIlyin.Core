module ThrottleTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.EdIlyin.Core


let inline equal (expected: 'T) (actual: 'T): unit =
    let assert' = importAll<obj> "assert"
    assert'?deepStrictEqual(actual, expected) |> ignore


[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative


it "throttle: simple function" <| fun () ->
    let result = ref 0
    let throttle = Throttle.start 5 1000
    let func () = 42

    do async
        {   let! x = throttle func
            do result := x
        }   |> Async.StartImmediate

    equal !result 42


it "throttle: async function" <| fun () ->
    let result = ref 0
    let throttle = Throttle.start 5 1000
    let func () = async {return 42}

    do async
        {   let! x = async.Bind (throttle func, id)
            do result := x
        }   |> Async.StartImmediate

    equal !result 42
