[<AutoOpen>]
module Fable.EdIlyin.Core.Basics


let curry func x y = func (x, y)


let first x _ = x


let flip func x y = func y x


let second _ x = x


let tuple x y = x, y


let uncurry func (x, y) = func x y
