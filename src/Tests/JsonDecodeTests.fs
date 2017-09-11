module JsonDecodeTests

open Fable.Core
open Fable.Core.JsInterop
open Fable.EdIlyin.Core


module JD = Fable.EdIlyin.Core.Json.Decode


module JE = Fable.EdIlyin.Core.Json.Encode


let equal (expected: 'T) (actual: 'T): unit =
    let assert' = JsInterop.importAll<obj> "assert"
    assert'?deepStrictEqual(actual, expected) |> ignore


[<Global>]
let it (msg: string) (f: unit -> unit): unit = jsNative


it "json decode: null" <| fun () ->
    JD.decodeString (JD.Null true) "null" |> equal (Ok true)


it "json decode: nullable int: 42" <| fun () ->
    JD.decodeString (JD.nullable JD.int) "42" |> equal (Some 42 |> Ok)


it "json decode: nullable int: null" <| fun () ->
    JD.decodeString (JD.nullable JD.int) "null" |> equal (Ok None)


it "json decode: index: 42" <| fun () ->
    JD.decodeString (JD.index 1 JD.int) "[12,42,43]" |> equal (Ok 42)


it "json decode: index: nullable int: 42" <| fun () ->
    JD.decodeString (JD.nullable JD.int |> JD.index 1) "[12,42,43]"
        |> equal (Some 42 |> Ok)


it "json decode: index: nullable int: null" <| fun () ->
    JD.decodeString (JD.nullable JD.int |> JD.index 1) "[12,null,43]"
        |> equal (Ok None)


it "json decode: index last element" <| fun () ->
    JD.decodeString (JD.nullable JD.int |> JD.index 2) "[12,null,43]"
        |> equal (Some 43 |> Ok)


it "json decode: index out of length" <| fun () ->
    JD.decodeString (JD.nullable JD.int |> JD.index 3) "[12,null,43]"
        |> equal
            (Error
                "Expecting a longer array. Need index 3, but instead got: \"[12,null,43]\""
            )


let floatFromString =
    JD.string
        |> Decode.andThen
            (fun s ->
                try float s |> Ok
                with e -> Decode.expectingButGot "a Float in String" s
                |> Decode.fromDecodeResult
            )


it "json decode: wrong field name" <| fun () ->
    JD.decodeString (JD.field "lowerAsk" floatFromString)
        "{\"last\":\"0.00007602\",\"lowestAsk\":\"0.00007602\"}"
        |> equal
            (Error
                "Expecting a String field 'lowerAsk', but instead got: \"{\\\"last\\\":\\\"0.00007602\\\",\\\"lowestAsk\\\":\\\"0.00007602\\\"}\""
            )


it "json decode: dict: wrong field name" <| fun () ->
    JD.decodeString (JD.field "lowerAsk" floatFromString |> JD.dict)
        """{"one":{"a":1,"b":2},"two":{"a":2,"b":3}}"""
        |> equal
            (Error
                "Expecting a String field 'lowerAsk', but instead got: \"{\\\"a\\\":1,\\\"b\\\":2}\""
            )
