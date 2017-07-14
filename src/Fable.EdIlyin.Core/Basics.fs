[<AutoOpen>]
module Fable.EdIlyin.Core.Basics


let (=>) x y = x, y


let curry4 (fn: 'a -> 'b -> 'c -> 'd -> 'e) =
    let first = fun (a: 'a) ->
        let second = fun (b: 'b) ->
            let third = fun (c: 'c) ->
                let fourth = fun (d: 'd) ->
                    let result = fn a b c d
                    result
                fourth
            third
        second
    first


let uncurry func (x, y) = func x y


let flip func x y = func y x


let curry func x y = func (x, y)