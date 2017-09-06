module Fable.EdIlyin.Core.Decode


type Label = string


type Unexpected = string


type ErrorMessage = string


type DecodeResult<'T> =
    | Decoded of 'T
    | ExpectingButGot of Label * Unexpected
    | ErrorMessage of ErrorMessage


type Decoder<'From,'To> = {
    decoder: 'From -> DecodeResult<'To>
    label: Label
}


let run decoder jsonValue =
    decoder.decoder jsonValue


let decode decoder source =
    run decoder source
        |> function
            | Decoded value -> Ok value

            | ExpectingButGot (expecting, got) ->
                sprintf "Expecting %s, but instead got: %A"
                    expecting
                    got
                    |> Error

            | ErrorMessage message -> Error message


let primitive label func =
    { decoder = func; label = label }


let fail error =
    primitive "anything" <| fun _ -> ErrorMessage error


let succeed value =
    primitive <| sprintf "%A" value <| fun _ -> Decoded value


let fromResult result =
    match result with
        | Error error -> fail error
        | Ok value -> succeed value


let getLabel decoder = decoder.label


let setLabel label decoder = { decoder with label = label }


let (<?>) decoder label = setLabel label decoder


let andThen func decoder =
    let label = getLabel decoder

    primitive label
        (fun input ->
            match run decoder input with
                | Decoded value -> run (func value) input

                | ExpectingButGot (expecting, got) ->
                    expecting => got |> ExpectingButGot

                | ErrorMessage message -> ErrorMessage message
        )


let (>>=) decoder func = andThen func decoder


let andMap decoder functionDecoder =
    functionDecoder >>= (fun func -> decoder >>= (func >> succeed))


let (<*>) fnDecoder decoder = andMap decoder fnDecoder


let map func decoder =
    succeed func
        <*> decoder
        <?> sprintf "{ %s }" (getLabel decoder)


let map2 func decoder1 decoder2 =
    succeed func
        <*> decoder1
        <*> decoder2
        <?> sprintf "{ %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)


let map3 func decoder1 decoder2 decoder3 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <?> sprintf "{ %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)


let map4 func decoder1 decoder2 decoder3 decoder4 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <?> sprintf "{ %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)


let map5 func decoder1 decoder2 decoder3 decoder4 decoder5 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <?> sprintf "{ %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)


let map6 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <?> sprintf "{ %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)


let map7 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <*> decoder7
        <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)
            (getLabel decoder7)


let map8 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    succeed func
        <*> decoder1
        <*> decoder2
        <*> decoder3
        <*> decoder4
        <*> decoder5
        <*> decoder6
        <*> decoder7
        <*> decoder8
        <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s, %s }"
            (getLabel decoder1)
            (getLabel decoder2)
            (getLabel decoder3)
            (getLabel decoder4)
            (getLabel decoder5)
            (getLabel decoder6)
            (getLabel decoder7)
            (getLabel decoder8)


let resultFromResult =
    function
        | Error x -> ErrorMessage x
        | Ok x -> Decoded x


let fromDecodeResult decodeResult =
    primitive "result" <| fun _ -> decodeResult


type Builder () =
    member this.Bind (m, f) = andThen f m
    member this.Return x = succeed x


let decoder = Builder ()
