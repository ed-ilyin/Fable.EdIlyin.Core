module Fable.EdIlyin.Core.Decode


type Expecting = string


type Got = string


type ErrorMessage = string


type Error =
    | ExpectingButGot of Expecting * Got
    | ErrorMessage of ErrorMessage


type Decoder<'From,'To> =
    Decoder of Expecting * ('From -> Result<'To,Error>)


let run (Decoder (_, decoder)) input = decoder input


let getLabel (Decoder (expecting, _)) = expecting


let decode decoder source =
    match run decoder source with
        | Ok output -> Ok output

        | Error (ExpectingButGot (expecting, got)) ->
            sprintf "Expecting %s, but instead got: %A" expecting got
                |> Error

        | Error (ErrorMessage error) -> Error error


let primitive expecting func = Decoder (expecting, func)


let fromFunction func = primitive "" func


let fail error = fromFunction <| fun _ -> ErrorMessage error |> Error


let succeed value = fromFunction <| fun _ -> Ok value


let expecting expecting (Decoder (_, decoder)) =
    primitive expecting decoder


let expectingButGot expecting got =
    ExpectingButGot (expecting, sprintf "%A" got) |> Error


let andThen func decoder =
    let label = getLabel decoder

    fun input ->
        match run decoder input with
            | Ok value -> run (func value) input
            | Error error -> Error error

    |> primitive label


type Builder () =
    member this.Bind (m, f) = andThen f m
    member this.Return m = succeed m


let andMap decoder functionDecoder =
    functionDecoder
        |> andThen (fun f -> decoder |> andThen (f >> succeed))


let map func decoder =
    succeed func |> andMap decoder


let map2 func decoder1 decoder2 =
    succeed func |> andMap decoder1 |> andMap decoder2


let map3 func decoder1 decoder2 decoder3 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3


let map4 func decoder1 decoder2 decoder3 decoder4 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3
        |> andMap decoder4


let map5 func decoder1 decoder2 decoder3 decoder4 decoder5 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3
        |> andMap decoder4
        |> andMap decoder5


let map6 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3
        |> andMap decoder4
        |> andMap decoder5
        |> andMap decoder6


let map7 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3
        |> andMap decoder4
        |> andMap decoder5
        |> andMap decoder6
        |> andMap decoder7


let map8 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3
        |> andMap decoder4
        |> andMap decoder5
        |> andMap decoder6
        |> andMap decoder7
        |> andMap decoder8


let map9 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
    succeed func
        |> andMap decoder1
        |> andMap decoder2
        |> andMap decoder3
        |> andMap decoder4
        |> andMap decoder5
        |> andMap decoder6
        |> andMap decoder7
        |> andMap decoder8
        |> andMap decoder9


let fromResult result = Result.unpack fail succeed result


let resultFromResult =
    function
        | Error x -> ErrorMessage x |> Error
        | Ok x -> Ok x


let result decoder = decode decoder >> Ok |> fromFunction


let errorMessage errorMessage = ErrorMessage errorMessage |> Error


let ok value = Ok value


let fromDecodeResult decodeResult =
    fromFunction <| fun _ -> decodeResult


let combineList decoderList =
    succeed List.empty
        |> List.foldBack (map2 (fun e l -> e::l)) decoderList


let maybe decoder =
    decode decoder
        >> Result.unpack (fun _ -> ok None) (Some >> ok)
        |> fromFunction


let withDefault fallback decoder =
    maybe decoder |> map (Option.withDefault fallback)


let orElse decoder2 decoder1 =
    decoder1
        |> result
        |> andThen
            (function
                | Error _ -> decoder2
                | Ok v -> Ok v |> fromResult
            )


let oneOf decoderList =
    List.reduce (flip orElse) decoderList
