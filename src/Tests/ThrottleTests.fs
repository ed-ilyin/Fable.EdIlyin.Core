module ThrottleTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.EdIlyin.Core
open Fable.Import
open Fable.PowerPack


let equal (expected: 'T) (actual: 'T): unit =
    let assert' = importAll<obj> "assert"
    assert'?deepStrictEqual(actual, expected) |> ignore


[<Global>]
let it (msg: string) (f: unit->JS.Promise<'T>): unit = jsNative


it "throttle: simple function" <| fun () ->
    let throttler = Throttle.start 1 1000
    let func () = 42

    async {
        let! x = Throttle.add throttler func
        return equal 42 x
    }
        |> Async.StartAsPromise


it "throttle: async function" <| fun () ->
    let throttler = Throttle.start 2 1000
    let func () = async {return 42}

    async {
        let! x = Throttle.add throttler func
        let! y = x
        return equal 42 y
    }
        |> Async.StartAsPromise


let multipleFunTest func () =
    let throttler = Throttle.start 3 100
    let quantity = 22

    async {
        let! times =
            List.init quantity
                (fun _ -> Throttle.add throttler |> func)
                |> Async.Parallel

        let results =
            times
                // |> Debug.log "times"
                |> Array.pairwise
                |> Array.map (fun (x,y) -> y - x |> int)
                // |> Debug.log "diffs"
                |> Seq.map2 (fun (f, t) x -> f <= x && x <= t)
                    (
                        let rec loop () =
                            seq {
                                yield 0, 0
                                yield 0, 10
                                yield 90, 110
                                yield! loop ()
                            }
                        loop ()
                    )
                // |> Debug.log "results"

        do equal (Seq.init (quantity - 1) <| fun _ -> true) results
    }
        |> Async.StartAsPromise


it "throttle: multiple simple functions"
    <| multipleFunTest
        (fun throttle -> throttle Throttle.nowMilliseconds)


it "throttle: multiple async functions"
    <| multipleFunTest
        (fun throttle ->
            let func x = async { return Throttle.nowMilliseconds x }
            async {
                let! x = throttle func
                return! x
            }
        )


type DifferentResult = | Int of int | String of string


it "throttle: couple of different functions" <| fun () ->
    let throttler = Throttle.start 4 100
    let throttle = Throttle.add throttler

    let x1, x2 = 42, "thirty two"
    let func1 () = x1
    let func2 () = x2

    async {
        let! result1 = func1 >> Int |> throttle
        let! result2 = func2 >> String |> throttle
        return equal (Int x1, String x2) (result1, result2)
    }
        |> Async.StartAsPromise
