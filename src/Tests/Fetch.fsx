let inline start () f =
    let inline innerFunc func = func ()
    innerFunc

let inline func1 () = 42
let inline func2 () = "sorok dva"

let inline run () =
    let throttle f = start () f
    let r1 = throttle func1
    let r2 = throttle func2
    r1, r2
