module Fable.EdIlyin.Core.Async


let inline (>>=) asyn func = async.Bind (asyn, func)


let inline map func asyn = asyn >>= (func >> async.Return)


let inline (|>>) asyn func = map func asyn
