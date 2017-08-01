'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var assert = require('assert');
var es6Promise = require('es6-promise');
require('isomorphic-fetch');

class Trampoline {
    static get maxTrampolineCallCount() {
        return 2000;
    }
    constructor() {
        this.callCount = 0;
    }
    incrementAndCheck() {
        return this.callCount++ > Trampoline.maxTrampolineCallCount;
    }
    hijack(f) {
        this.callCount = 0;
        setTimeout(f, 0);
    }
}
function protectedCont(f) {
    return (ctx) => {
        if (ctx.cancelToken.isCancelled) {
            ctx.onCancel("cancelled");
        }
        else if (ctx.trampoline.incrementAndCheck()) {
            ctx.trampoline.hijack(() => {
                try {
                    f(ctx);
                }
                catch (err) {
                    ctx.onError(err);
                }
            });
        }
        else {
            try {
                f(ctx);
            }
            catch (err) {
                ctx.onError(err);
            }
        }
    };
}
function protectedBind(computation, binder) {
    return protectedCont((ctx) => {
        computation({
            onSuccess: (x) => {
                try {
                    binder(x)(ctx);
                }
                catch (ex) {
                    ctx.onError(ex);
                }
            },
            onError: ctx.onError,
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline,
        });
    });
}
function protectedReturn(value) {
    return protectedCont((ctx) => ctx.onSuccess(value));
}
class AsyncBuilder {
    Bind(computation, binder) {
        return protectedBind(computation, binder);
    }
    Combine(computation1, computation2) {
        return this.Bind(computation1, () => computation2);
    }
    Delay(generator) {
        return protectedCont((ctx) => generator()(ctx));
    }
    For(sequence, body) {
        const iter = sequence[Symbol.iterator]();
        let cur = iter.next();
        return this.While(() => !cur.done, this.Delay(() => {
            const res = body(cur.value);
            cur = iter.next();
            return res;
        }));
    }
    Return(value) {
        return protectedReturn(value);
    }
    ReturnFrom(computation) {
        return computation;
    }
    TryFinally(computation, compensation) {
        return protectedCont((ctx) => {
            computation({
                onSuccess: (x) => {
                    compensation();
                    ctx.onSuccess(x);
                },
                onError: (x) => {
                    compensation();
                    ctx.onError(x);
                },
                onCancel: (x) => {
                    compensation();
                    ctx.onCancel(x);
                },
                cancelToken: ctx.cancelToken,
                trampoline: ctx.trampoline,
            });
        });
    }
    TryWith(computation, catchHandler) {
        return protectedCont((ctx) => {
            computation({
                onSuccess: ctx.onSuccess,
                onCancel: ctx.onCancel,
                cancelToken: ctx.cancelToken,
                trampoline: ctx.trampoline,
                onError: (ex) => {
                    try {
                        catchHandler(ex)(ctx);
                    }
                    catch (ex2) {
                        ctx.onError(ex2);
                    }
                },
            });
        });
    }
    Using(resource, binder) {
        return this.TryFinally(binder(resource), () => resource.Dispose());
    }
    While(guard, computation) {
        if (guard()) {
            return this.Bind(computation, () => this.While(guard, computation));
        }
        else {
            return this.Return(void 0);
        }
    }
    Zero() {
        return protectedCont((ctx) => ctx.onSuccess(void 0));
    }
}
const singleton = new AsyncBuilder();

const types = new Map();
function setType(fullName, cons) {
    types.set(fullName, cons);
}

var _Symbol = {
    reflection: Symbol("reflection"),
};

class NonDeclaredType {
    constructor(kind, definition, generics) {
        this.kind = kind;
        this.definition = definition;
        this.generics = generics;
    }
    Equals(other) {
        if (this.kind === other.kind && this.definition === other.definition) {
            return typeof this.generics === "object"
                ? equalsRecords(this.generics, other.generics)
                : this.generics === other.generics;
        }
        return false;
    }
}
const Any = new NonDeclaredType("Any");
const Unit = new NonDeclaredType("Unit");


function FableFunction(types) {
    return new NonDeclaredType("Function", null, types);
}
function GenericParam(definition) {
    return new NonDeclaredType("GenericParam", definition);
}
function Interface(definition) {
    return new NonDeclaredType("Interface", definition);
}
function makeGeneric(typeDef, genArgs) {
    return new NonDeclaredType("GenericType", typeDef, genArgs);
}

/**
 * Returns the parent if this is a declared generic type or the argument otherwise.
 * Attention: Unlike .NET this doesn't throw an exception if type is not generic.
 */


function hasInterface(obj, interfaceName) {
    if (interfaceName === "System.Collections.Generic.IEnumerable") {
        return typeof obj[Symbol.iterator] === "function";
    }
    else if (typeof obj[_Symbol.reflection] === "function") {
        const interfaces = obj[_Symbol.reflection]().interfaces;
        return Array.isArray(interfaces) && interfaces.indexOf(interfaceName) > -1;
    }
    return false;
}
/**
 * Returns:
 * - Records: array with names of fields
 * - Classes: array with names of getters
 * - Nulls and unions: empty array
 * - JS Objects: The result of calling Object.getOwnPropertyNames
 */
function getPropertyNames(obj) {
    if (obj == null) {
        return [];
    }
    const propertyMap = typeof obj[_Symbol.reflection] === "function" ? obj[_Symbol.reflection]().properties || [] : obj;
    return Object.getOwnPropertyNames(propertyMap);
}

function toString(obj, quoteStrings = false) {
    function isObject(x) {
        return x !== null && typeof x === "object" && !(x instanceof Number)
            && !(x instanceof String) && !(x instanceof Boolean);
    }
    if (obj == null || typeof obj === "number") {
        return String(obj);
    }
    if (typeof obj === "string") {
        return quoteStrings ? JSON.stringify(obj) : obj;
    }
    if (typeof obj.ToString === "function") {
        return obj.ToString();
    }
    if (hasInterface(obj, "FSharpUnion")) {
        const info = obj[_Symbol.reflection]();
        const uci = info.cases[obj.tag];
        switch (uci.length) {
            case 1:
                return uci[0];
            case 2:
                // For simplicity let's always use parens, in .NET they're ommitted in some cases
                return uci[0] + " (" + toString(obj.data, true) + ")";
            default:
                return uci[0] + " (" + obj.data.map((x) => toString(x, true)).join(",") + ")";
        }
    }
    try {
        return JSON.stringify(obj, (k, v) => {
            return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v)
                : v && typeof v.ToString === "function" ? toString(v) : v;
        });
    }
    catch (err) {
        // Fallback for objects with circular references
        return "{" + Object.getOwnPropertyNames(obj).map((k) => k + ": " + String(obj[k])).join(", ") + "}";
    }
}
function hash(x) {
    if (x != null && typeof x.GetHashCode === "function") {
        return x.GetHashCode();
    }
    else {
        const s = JSON.stringify(x);
        let h = 5381;
        let i = 0;
        const len = s.length;
        while (i < len) {
            h = (h * 33) ^ s.charCodeAt(i++);
        }
        return h;
    }
}
function equals(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return true;
    }
    else if (x == null) {
        return y == null;
    }
    else if (y == null) {
        return false;
    }
    else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) {
        return false;
        // Equals override or IEquatable implementation
    }
    else if (typeof x.Equals === "function") {
        return x.Equals(y);
    }
    else if (Array.isArray(x)) {
        if (x.length !== y.length) {
            return false;
        }
        for (let i = 0; i < x.length; i++) {
            if (!equals(x[i], y[i])) {
                return false;
            }
        }
        return true;
    }
    else if (ArrayBuffer.isView(x)) {
        if (x.byteLength !== y.byteLength) {
            return false;
        }
        const dv1 = new DataView(x.buffer);
        const dv2 = new DataView(y.buffer);
        for (let i = 0; i < x.byteLength; i++) {
            if (dv1.getUint8(i) !== dv2.getUint8(i)) {
                return false;
            }
        }
        return true;
    }
    else if (x instanceof Date) {
        return x.getTime() === y.getTime();
    }
    else {
        return false;
    }
}

function compare(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return 0;
    }
    else if (x == null) {
        return y == null ? 0 : -1;
    }
    else if (y == null) {
        return 1; // everything is bigger than null
    }
    else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) {
        return -1;
        // Some types (see Long.ts) may just implement the function and not the interface
        // else if (hasInterface(x, "System.IComparable"))
    }
    else if (typeof x.CompareTo === "function") {
        return x.CompareTo(y);
    }
    else if (Array.isArray(x)) {
        if (x.length !== y.length) {
            return x.length < y.length ? -1 : 1;
        }
        for (let i = 0, j = 0; i < x.length; i++) {
            j = compare(x[i], y[i]);
            if (j !== 0) {
                return j;
            }
        }
        return 0;
    }
    else if (ArrayBuffer.isView(x)) {
        if (x.byteLength !== y.byteLength) {
            return x.byteLength < y.byteLength ? -1 : 1;
        }
        const dv1 = new DataView(x.buffer);
        const dv2 = new DataView(y.buffer);
        for (let i = 0, b1 = 0, b2 = 0; i < x.byteLength; i++) {
            b1 = dv1.getUint8(i), b2 = dv2.getUint8(i);
            if (b1 < b2) {
                return -1;
            }
            if (b1 > b2) {
                return 1;
            }
        }
        return 0;
    }
    else if (x instanceof Date) {
        const xtime = x.getTime();
        const ytime = y.getTime();
        return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
    }
    else if (typeof x === "object") {
        const xhash = hash(x);
        const yhash = hash(y);
        if (xhash === yhash) {
            return equals(x, y) ? 0 : -1;
        }
        else {
            return xhash < yhash ? -1 : 1;
        }
    }
    else {
        return x < y ? -1 : 1;
    }
}
function equalsRecords(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return true;
    }
    else {
        const keys = getPropertyNames(x);
        for (const key of keys) {
            if (!equals(x[key], y[key])) {
                return false;
            }
        }
        return true;
    }
}
function compareRecords(x, y) {
    // Optimization if they are referencially equal
    if (x === y) {
        return 0;
    }
    else {
        const keys = getPropertyNames(x);
        for (const key of keys) {
            const res = compare(x[key], y[key]);
            if (res !== 0) {
                return res;
            }
        }
        return 0;
    }
}
function equalsUnions(x, y) {
    return x === y || (x.tag === y.tag && equals(x.data, y.data));
}
function compareUnions(x, y) {
    if (x === y) {
        return 0;
    }
    else {
        const res = x.tag < y.tag ? -1 : (x.tag > y.tag ? 1 : 0);
        return res !== 0 ? res : compare(x.data, y.data);
    }
}

// tslint forbids non-arrow functions, but it's
// necessary here to use the arguments object
/* tslint:disable */

/* tslint:enable */
const CaseRules = {
    None: 0,
    LowerFirst: 1,
};
function isList(o) {
    if (o != null) {
        if (typeof o[_Symbol.reflection] === "function") {
            return o[_Symbol.reflection]().type === "Microsoft.FSharp.Collections.FSharpList";
        }
    }
    return false;
}
function createObj(fields, caseRule = CaseRules.None, casesCache) {
    const iter = fields[Symbol.iterator]();
    let cur = iter.next();
    const o = {};
    while (!cur.done) {
        const value = cur.value;
        if (Array.isArray(value)) {
            o[value[0]] = value[1];
        }
        else {
            casesCache = casesCache || new Map();
            const proto = Object.getPrototypeOf(value);
            let cases = casesCache.get(proto);
            if (cases == null) {
                if (typeof proto[_Symbol.reflection] === "function") {
                    cases = proto[_Symbol.reflection]().cases;
                    casesCache.set(proto, cases);
                }
            }
            const caseInfo = (cases != null) ? cases[value.tag] : null;
            if (Array.isArray(caseInfo)) {
                let key = caseInfo[0];
                if (caseRule === CaseRules.LowerFirst) {
                    key = key[0].toLowerCase() + key.substr(1);
                }
                o[key] = caseInfo.length === 1
                    ? true
                    : (isList(value.data) ? createObj(value.data, caseRule, casesCache) : value.data);
            }
            else {
                throw new Error("Cannot infer key and value of " + value);
            }
        }
        cur = iter.next();
    }
    return o;
}

function choice1Of2(v) {
    return new Choice(0, v);
}
function choice2Of2(v) {
    return new Choice(1, v);
}
class Choice {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }
    get valueIfChoice1() {
        return this.tag === 0 ? this.data : null;
    }
    get valueIfChoice2() {
        return this.tag === 1 ? this.data : null;
    }
    Equals(other) {
        return equalsUnions(this, other);
    }
    CompareTo(other) {
        return compareUnions(this, other);
    }
    [_Symbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Core.FSharpChoice",
            interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
            cases: [["Choice1Of2", Any], ["Choice2Of2", Any]],
        };
    }
}

function map$1(f, source, target) {
    for (let i = 0; i < source.length; i++) {
        target[i] = f(source[i]);
    }
    return target;
}

// This module is split from List.ts to prevent cyclic dependencies
function ofArray$1(args, base) {
    let acc = base || new List();
    for (let i = args.length - 1; i >= 0; i--) {
        acc = new List(args[i], acc);
    }
    return acc;
}
class List {
    constructor(head, tail) {
        this.head = head;
        this.tail = tail;
    }
    ToString() {
        return "[" + Array.from(this).map((x) => toString(x)).join("; ") + "]";
    }
    Equals(x) {
        // Optimization if they are referencially equal
        if (this === x) {
            return true;
        }
        else {
            const iter1 = this[Symbol.iterator]();
            const iter2 = x[Symbol.iterator]();
            while (true) {
                const cur1 = iter1.next();
                const cur2 = iter2.next();
                if (cur1.done) {
                    return cur2.done ? true : false;
                }
                else if (cur2.done) {
                    return false;
                }
                else if (!equals(cur1.value, cur2.value)) {
                    return false;
                }
            }
        }
    }
    CompareTo(x) {
        // Optimization if they are referencially equal
        if (this === x) {
            return 0;
        }
        else {
            let acc = 0;
            const iter1 = this[Symbol.iterator]();
            const iter2 = x[Symbol.iterator]();
            while (true) {
                const cur1 = iter1.next();
                const cur2 = iter2.next();
                if (cur1.done) {
                    return cur2.done ? acc : -1;
                }
                else if (cur2.done) {
                    return 1;
                }
                else {
                    acc = compare(cur1.value, cur2.value);
                    if (acc !== 0) {
                        return acc;
                    }
                }
            }
        }
    }
    get length() {
        let cur = this;
        let acc = 0;
        while (cur.tail != null) {
            cur = cur.tail;
            acc++;
        }
        return acc;
    }
    [Symbol.iterator]() {
        let cur = this;
        return {
            next: () => {
                const tmp = cur;
                cur = cur.tail;
                return { done: tmp.tail == null, value: tmp.head };
            },
        };
    }
    //   append(ys: List<T>): List<T> {
    //     return append(this, ys);
    //   }
    //   choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
    //     return choose(f, this);
    //   }
    //   collect<U>(f: (x: T) => List<U>): List<U> {
    //     return collect(f, this);
    //   }
    //   filter(f: (x: T) => boolean): List<T> {
    //     return filter(f, this);
    //   }
    //   where(f: (x: T) => boolean): List<T> {
    //     return filter(f, this);
    //   }
    //   map<U>(f: (x: T) => U): List<U> {
    //     return map(f, this);
    //   }
    //   mapIndexed<U>(f: (i: number, x: T) => U): List<U> {
    //     return mapIndexed(f, this);
    //   }
    //   partition(f: (x: T) => boolean): [List<T>, List<T>] {
    //     return partition(f, this) as [List<T>, List<T>];
    //   }
    //   reverse(): List<T> {
    //     return reverse(this);
    //   }
    //   slice(lower: number, upper: number): List<T> {
    //     return slice(lower, upper, this);
    //   }
    [_Symbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Collections.FSharpList",
            interfaces: ["System.IEquatable", "System.IComparable"],
        };
    }
}

function append(xs, ys) {
    return delay(() => {
        let firstDone = false;
        const i = xs[Symbol.iterator]();
        let iters = [i, null];
        return unfold(() => {
            let cur;
            if (!firstDone) {
                cur = iters[0].next();
                if (!cur.done) {
                    return [cur.value, iters];
                }
                else {
                    firstDone = true;
                    iters = [null, ys[Symbol.iterator]()];
                }
            }
            cur = iters[1].next();
            return !cur.done ? [cur.value, iters] : null;
        }, iters);
    });
}






function delay(f) {
    return {
        [Symbol.iterator]: () => f()[Symbol.iterator](),
    };
}










function fold(f, acc, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return xs.reduce(f, acc);
    }
    else {
        let cur;
        for (let i = 0, iter = xs[Symbol.iterator]();; i++) {
            cur = iter.next();
            if (cur.done) {
                break;
            }
            acc = f(acc, cur.value, i);
        }
        return acc;
    }
}
function foldBack(f, xs, acc) {
    const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
    for (let i = arr.length - 1; i >= 0; i--) {
        acc = f(arr[i], acc, i);
    }
    return acc;
}






function initialize(n, f) {
    return delay(() => unfold((i) => i < n ? [f(i), i + 1] : null, 0));
}



function iterate(f, xs) {
    fold((_, x) => f(x), null, xs);
}






// A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442

function map(f, xs) {
    return delay(() => unfold((iter) => {
        const cur = iter.next();
        return !cur.done ? [f(cur.value), iter] : null;
    }, xs[Symbol.iterator]()));
}


function map2(f, xs, ys) {
    return delay(() => {
        const iter1 = xs[Symbol.iterator]();
        const iter2 = ys[Symbol.iterator]();
        return unfold(() => {
            const cur1 = iter1.next();
            const cur2 = iter2.next();
            return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
        });
    });
}









function pairwise(xs) {
    return skip(2, scan((last, next) => [last[1], next], [0, 0], xs));
}









function scan(f, seed, xs) {
    return delay(() => {
        const iter = xs[Symbol.iterator]();
        return unfold((acc) => {
            if (acc == null) {
                return [seed, seed];
            }
            const cur = iter.next();
            if (!cur.done) {
                acc = f(acc, cur.value);
                return [acc, acc];
            }
            return void 0;
        }, null);
    });
}

function singleton$1(y) {
    return unfold((x) => x != null ? [x, null] : null, y);
}
function skip(n, xs) {
    return {
        [Symbol.iterator]: () => {
            const iter = xs[Symbol.iterator]();
            for (let i = 1; i <= n; i++) {
                if (iter.next().done) {
                    throw new Error("Seq has not enough elements");
                }
            }
            return iter;
        },
    };
}





function take(n, xs, truncate = false) {
    return delay(() => {
        const iter = xs[Symbol.iterator]();
        return unfold((i) => {
            if (i < n) {
                const cur = iter.next();
                if (!cur.done) {
                    return [cur.value, i + 1];
                }
                if (!truncate) {
                    throw new Error("Seq has not enough elements");
                }
            }
            return void 0;
        }, 0);
    });
}
function truncate(n, xs) {
    return take(n, xs, true);
}











function unfold(f, acc) {
    return {
        [Symbol.iterator]: () => {
            return {
                next: () => {
                    const res = f(acc);
                    if (res != null) {
                        acc = res[1];
                        return { done: false, value: res[0] };
                    }
                    return { done: true };
                },
            };
        },
    };
}

function emptyContinuation(x) {
    // NOP
}
function awaitPromise(p) {
    return fromContinuations((conts) => p.then(conts[0]).catch((err) => (err === "cancelled" ? conts[2] : conts[1])(err)));
}

const defaultCancellationToken = { isCancelled: false };
function catchAsync(work) {
    return protectedCont((ctx) => {
        work({
            onSuccess: (x) => ctx.onSuccess(choice1Of2(x)),
            onError: (ex) => ctx.onSuccess(choice2Of2(ex)),
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline,
        });
    });
}
function fromContinuations(f) {
    return protectedCont((ctx) => f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
}

function parallel(computations) {
    return awaitPromise(Promise.all(map((w) => startAsPromise(w), computations)));
}
function sleep(millisecondsDueTime) {
    return protectedCont((ctx) => {
        setTimeout(() => ctx.cancelToken.isCancelled ?
            ctx.onCancel("cancelled") : ctx.onSuccess(void 0), millisecondsDueTime);
    });
}
function start(computation, cancellationToken) {
    return startWithContinuations(computation, cancellationToken);
}
function startImmediate(computation, cancellationToken) {
    return start(computation, cancellationToken);
}
function startWithContinuations(computation, continuation, exceptionContinuation, cancellationContinuation, cancelToken) {
    if (typeof continuation !== "function") {
        cancelToken = continuation;
        continuation = null;
    }
    const trampoline = new Trampoline();
    computation({
        onSuccess: continuation ? continuation : emptyContinuation,
        onError: exceptionContinuation ? exceptionContinuation : emptyContinuation,
        onCancel: cancellationContinuation ? cancellationContinuation : emptyContinuation,
        cancelToken: cancelToken ? cancelToken : defaultCancellationToken,
        trampoline,
    });
}
function startAsPromise(computation, cancellationToken) {
    return new Promise((resolve, reject) => startWithContinuations(computation, resolve, reject, reject, cancellationToken ? cancellationToken : defaultCancellationToken));
}

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies

function append$1(xs, ys) {
    return fold((acc, x) => new List(x, acc), ys, reverse$1(xs));
}


// TODO: should be xs: Iterable<List<T>>



function initialize$1(n, f) {
    if (n < 0) {
        throw new Error("List length must be non-negative");
    }
    let xs = new List();
    for (let i = 1; i <= n; i++) {
        xs = new List(f(n - i), xs);
    }
    return xs;
}





function reverse$1(xs) {
    return fold((acc, x) => new List(x, acc), new List(), xs);
}


/* ToDo: instance unzip() */

/* ToDo: instance unzip3() */

// TODO verify that this matches the behavior of .NET
const parseRadix10 = /^ *([\+\-]?[0-9]+) *$/;
// TODO verify that this matches the behavior of .NET
const parseRadix16 = /^ *([\+\-]?[0-9a-fA-F]+) *$/;
function isValid(s, radix) {
    if (s != null) {
        if (radix === 16) {
            return parseRadix16.exec(s);
        }
        else if (radix <= 10) {
            return parseRadix10.exec(s);
        }
    }
    return null;
}
// TODO does this perfectly match the .NET behavior ?

// Source: https://github.com/dcodeIO/long.js/blob/master/LICENSE
// tslint:disable:curly
// tslint:disable:member-access
// tslint:disable:member-ordering
// The internal representation of a long is the two given signed, 32-bit values.
// We use 32-bit pieces because these are the size of integers on which
// Javascript performs bit-operations.  For operations like addition and
// multiplication, we split each number into 16 bit pieces, which can easily be
// multiplied within Javascript's floating-point representation without overflow
// or change in sign.
//
// In the algorithms below, we frequently reduce the negative case to the
// positive case by negating the input(s) and then post-processing the result.
// Note that we must ALWAYS check specially whether those values are MIN_VALUE
// (-2^63) because -MIN_VALUE == MIN_VALUE (since 2^63 cannot be represented as
// a positive number, it overflows back into a negative).  Not handling this
// case would often result in infinite recursion.
//
// Common constant values ZERO, ONE, NEG_ONE, etc. are defined below the from*
// methods on which they depend.
/**
 * @class A Long class for representing a 64 bit two's-complement integer value.
 */
class Long {
    /**
     * Constructs a 64 bit two's-complement integer, given its low and high 32 bit values as *signed* integers.
     *  See the from* functions below for more convenient ways of constructing Longs.
     * @param {number} low The low (signed) 32 bits of the long
     * @param {number} high The high (signed) 32 bits of the long
     * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
     */
    constructor(low, high, unsigned) {
        /**
         * Tests if this Long's value equals the specified's. This is an alias of {@link Long#equals}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.eq = this.equals;
        /**
         * Tests if this Long's value differs from the specified's. This is an alias of {@link Long#notEquals}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.neq = this.notEquals;
        /**
         * Tests if this Long's value is less than the specified's. This is an alias of {@link Long#lessThan}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.lt = this.lessThan;
        /**
         * Tests if this Long's value is less than or equal the specified's.
         * This is an alias of {@link Long#lessThanOrEqual}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.lte = this.lessThanOrEqual;
        /**
         * Tests if this Long's value is greater than the specified's. This is an alias of {@link Long#greaterThan}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.gt = this.greaterThan;
        /**
         * Tests if this Long's value is greater than or equal the specified's.
         * This is an alias of {@link Long#greaterThanOrEqual}.
         * @param {!Long|number|string} other Other value
         * @returns {boolean}
         */
        this.gte = this.greaterThanOrEqual;
        /**
         * Compares this Long's value with the specified's. This is an alias of {@link Long#compare}.
         * @param {!Long|number|string} other Other value
         * @returns {number} 0 if they are the same, 1 if the this is greater and -1
         *  if the given one is greater
         */
        this.comp = this.compare;
        /**
         * Negates this Long's value. This is an alias of {@link Long#negate}.
         * @returns {!Long} Negated Long
         */
        this.neg = this.negate;
        /**
         * Returns this Long's absolute value. This is an alias of {@link Long#absolute}.
         * @returns {!Long} Absolute Long
         */
        this.abs = this.absolute;
        /**
         * Returns the difference of this and the specified  This is an alias of {@link Long#subtract}.
         * @param {!Long|number|string} subtrahend Subtrahend
         * @returns {!Long} Difference
         */
        this.sub = this.subtract;
        /**
         * Returns the product of this and the specified  This is an alias of {@link Long#multiply}.
         * @param {!Long|number|string} multiplier Multiplier
         * @returns {!Long} Product
         */
        this.mul = this.multiply;
        /**
         * Returns this Long divided by the specified. This is an alias of {@link Long#divide}.
         * @param {!Long|number|string} divisor Divisor
         * @returns {!Long} Quotient
         */
        this.div = this.divide;
        /**
         * Returns this Long modulo the specified. This is an alias of {@link Long#modulo}.
         * @param {!Long|number|string} divisor Divisor
         * @returns {!Long} Remainder
         */
        this.mod = this.modulo;
        /**
         * Returns this Long with bits shifted to the left by the given amount. This is an alias of {@link Long#shiftLeft}.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */
        this.shl = this.shiftLeft;
        /**
         * Returns this Long with bits arithmetically shifted to the right by the given amount.
         * This is an alias of {@link Long#shiftRight}.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */
        this.shr = this.shiftRight;
        /**
         * Returns this Long with bits logically shifted to the right by the given amount.
         * This is an alias of {@link Long#shiftRightUnsigned}.
         * @param {number|!Long} numBits Number of bits
         * @returns {!Long} Shifted Long
         */
        this.shru = this.shiftRightUnsigned;
        // Aliases for compatibility with Fable
        this.Equals = this.equals;
        this.CompareTo = this.compare;
        this.ToString = this.toString;
        this.low = low | 0;
        this.high = high | 0;
        this.unsigned = !!unsigned;
    }
    /**
     * Converts the Long to a 32 bit integer, assuming it is a 32 bit integer.
     * @returns {number}
     */
    toInt() {
        return this.unsigned ? this.low >>> 0 : this.low;
    }
    /**
     * Converts the Long to a the nearest floating-point representation of this value (double, 53 bit mantissa).
     * @returns {number}
     */
    toNumber() {
        if (this.unsigned)
            return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
        return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
    }
    /**
     * Converts the Long to a string written in the specified radix.
     * @param {number=} radix Radix (2-36), defaults to 10
     * @returns {string}
     * @override
     * @throws {RangeError} If `radix` is out of range
     */
    toString(radix = 10) {
        radix = radix || 10;
        if (radix < 2 || 36 < radix)
            throw RangeError("radix");
        if (this.isZero())
            return "0";
        if (this.isNegative()) {
            if (this.eq(MIN_VALUE)) {
                // We need to change the Long value before it can be negated, so we remove
                // the bottom-most digit in this base and then recurse to do the rest.
                const radixLong = fromNumber(radix);
                const div = this.div(radixLong);
                const rem1 = div.mul(radixLong).sub(this);
                return div.toString(radix) + rem1.toInt().toString(radix);
            }
            else
                return "-" + this.neg().toString(radix);
        }
        // Do several (6) digits each time through the loop, so as to
        // minimize the calls to the very expensive emulated div.
        const radixToPower = fromNumber(pow_dbl(radix, 6), this.unsigned);
        let rem = this;
        let result = "";
        while (true) {
            const remDiv = rem.div(radixToPower);
            const intval = rem.sub(remDiv.mul(radixToPower)).toInt() >>> 0;
            let digits = intval.toString(radix);
            rem = remDiv;
            if (rem.isZero())
                return digits + result;
            else {
                while (digits.length < 6)
                    digits = "0" + digits;
                result = "" + digits + result;
            }
        }
    }
    /**
     * Gets the high 32 bits as a signed integer.
     * @returns {number} Signed high bits
     */
    getHighBits() {
        return this.high;
    }
    /**
     * Gets the high 32 bits as an unsigned integer.
     * @returns {number} Unsigned high bits
     */
    getHighBitsUnsigned() {
        return this.high >>> 0;
    }
    /**
     * Gets the low 32 bits as a signed integer.
     * @returns {number} Signed low bits
     */
    getLowBits() {
        return this.low;
    }
    /**
     * Gets the low 32 bits as an unsigned integer.
     * @returns {number} Unsigned low bits
     */
    getLowBitsUnsigned() {
        return this.low >>> 0;
    }
    /**
     * Gets the number of bits needed to represent the absolute value of this
     * @returns {number}
     */
    getNumBitsAbs() {
        if (this.isNegative())
            return this.eq(MIN_VALUE) ? 64 : this.neg().getNumBitsAbs();
        const val = this.high !== 0 ? this.high : this.low;
        let bit;
        for (bit = 31; bit > 0; bit--)
            if ((val & (1 << bit)) !== 0)
                break;
        return this.high !== 0 ? bit + 33 : bit + 1;
    }
    /**
     * Tests if this Long's value equals zero.
     * @returns {boolean}
     */
    isZero() {
        return this.high === 0 && this.low === 0;
    }
    /**
     * Tests if this Long's value is negative.
     * @returns {boolean}
     */
    isNegative() {
        return !this.unsigned && this.high < 0;
    }
    /**
     * Tests if this Long's value is positive.
     * @returns {boolean}
     */
    isPositive() {
        return this.unsigned || this.high >= 0;
    }
    /**
     * Tests if this Long's value is odd.
     * @returns {boolean}
     */
    isOdd() {
        return (this.low & 1) === 1;
    }
    /**
     * Tests if this Long's value is even.
     * @returns {boolean}
     */
    isEven() {
        return (this.low & 1) === 0;
    }
    /**
     * Tests if this Long's value equals the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    equals(other) {
        if (!isLong(other))
            other = fromValue(other);
        if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
            return false;
        return this.high === other.high && this.low === other.low;
    }
    /**
     * Tests if this Long's value differs from the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    notEquals(other) {
        return !this.eq(/* validates */ other);
    }
    /**
     * Tests if this Long's value is less than the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    lessThan(other) {
        return this.comp(/* validates */ other) < 0;
    }
    /**
     * Tests if this Long's value is less than or equal the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    lessThanOrEqual(other) {
        return this.comp(/* validates */ other) <= 0;
    }
    /**
     * Tests if this Long's value is greater than the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    greaterThan(other) {
        return this.comp(/* validates */ other) > 0;
    }
    /**
     * Tests if this Long's value is greater than or equal the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    greaterThanOrEqual(other) {
        return this.comp(/* validates */ other) >= 0;
    }
    /**
     * Compares this Long's value with the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {number} 0 if they are the same, 1 if the this is greater and -1
     *  if the given one is greater
     */
    compare(other) {
        if (!isLong(other))
            other = fromValue(other);
        if (this.eq(other))
            return 0;
        const thisNeg = this.isNegative();
        const otherNeg = other.isNegative();
        if (thisNeg && !otherNeg)
            return -1;
        if (!thisNeg && otherNeg)
            return 1;
        // At this point the sign bits are the same
        if (!this.unsigned)
            return this.sub(other).isNegative() ? -1 : 1;
        // Both are positive if at least one is unsigned
        return (other.high >>> 0) > (this.high >>> 0) ||
            (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
    }
    /**
     * Negates this Long's value.
     * @returns {!Long} Negated Long
     */
    negate() {
        if (!this.unsigned && this.eq(MIN_VALUE))
            return MIN_VALUE;
        return this.not().add(ONE);
    }
    /**
     * Returns this Long's absolute value.
     * @returns {!Long} Absolute Long
     */
    absolute() {
        if (!this.unsigned && this.isNegative())
            return this.negate();
        else
            return this;
    }
    /**
     * Returns the sum of this and the specified
     * @param {!Long|number|string} addend Addend
     * @returns {!Long} Sum
     */
    add(addend) {
        if (!isLong(addend))
            addend = fromValue(addend);
        // Divide each number into 4 chunks of 16 bits, and then sum the chunks.
        const a48 = this.high >>> 16;
        const a32 = this.high & 0xFFFF;
        const a16 = this.low >>> 16;
        const a00 = this.low & 0xFFFF;
        const b48 = addend.high >>> 16;
        const b32 = addend.high & 0xFFFF;
        const b16 = addend.low >>> 16;
        const b00 = addend.low & 0xFFFF;
        let c48 = 0;
        let c32 = 0;
        let c16 = 0;
        let c00 = 0;
        c00 += a00 + b00;
        c16 += c00 >>> 16;
        c00 &= 0xFFFF;
        c16 += a16 + b16;
        c32 += c16 >>> 16;
        c16 &= 0xFFFF;
        c32 += a32 + b32;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c48 += a48 + b48;
        c48 &= 0xFFFF;
        return fromBits((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
    }
    /**
     * Returns the difference of this and the specified
     * @param {!Long|number|string} subtrahend Subtrahend
     * @returns {!Long} Difference
     */
    subtract(subtrahend) {
        if (!isLong(subtrahend))
            subtrahend = fromValue(subtrahend);
        return this.add(subtrahend.neg());
    }
    /**
     * Returns the product of this and the specified
     * @param {!Long|number|string} multiplier Multiplier
     * @returns {!Long} Product
     */
    multiply(multiplier) {
        if (this.isZero())
            return ZERO;
        if (!isLong(multiplier))
            multiplier = fromValue(multiplier);
        if (multiplier.isZero())
            return ZERO;
        if (this.eq(MIN_VALUE))
            return multiplier.isOdd() ? MIN_VALUE : ZERO;
        if (multiplier.eq(MIN_VALUE))
            return this.isOdd() ? MIN_VALUE : ZERO;
        if (this.isNegative()) {
            if (multiplier.isNegative())
                return this.neg().mul(multiplier.neg());
            else
                return this.neg().mul(multiplier).neg();
        }
        else if (multiplier.isNegative())
            return this.mul(multiplier.neg()).neg();
        // If both longs are small, use float multiplication
        if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
            return fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);
        // Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
        // We can skip products that would overflow.
        const a48 = this.high >>> 16;
        const a32 = this.high & 0xFFFF;
        const a16 = this.low >>> 16;
        const a00 = this.low & 0xFFFF;
        const b48 = multiplier.high >>> 16;
        const b32 = multiplier.high & 0xFFFF;
        const b16 = multiplier.low >>> 16;
        const b00 = multiplier.low & 0xFFFF;
        let c48 = 0;
        let c32 = 0;
        let c16 = 0;
        let c00 = 0;
        c00 += a00 * b00;
        c16 += c00 >>> 16;
        c00 &= 0xFFFF;
        c16 += a16 * b00;
        c32 += c16 >>> 16;
        c16 &= 0xFFFF;
        c16 += a00 * b16;
        c32 += c16 >>> 16;
        c16 &= 0xFFFF;
        c32 += a32 * b00;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c32 += a16 * b16;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c32 += a00 * b32;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
        c48 &= 0xFFFF;
        return fromBits((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
    }
    /**
     * Returns this Long divided by the specified. The result is signed if this Long is signed or
     *  unsigned if this Long is unsigned.
     * @param {!Long|number|string} divisor Divisor
     * @returns {!Long} Quotient
     */
    divide(divisor) {
        if (!isLong(divisor))
            divisor = fromValue(divisor);
        if (divisor.isZero())
            throw Error("division by zero");
        if (this.isZero())
            return this.unsigned ? UZERO : ZERO;
        let rem = ZERO;
        let res = ZERO;
        if (!this.unsigned) {
            // This section is only relevant for signed longs and is derived from the
            // closure library as a whole.
            if (this.eq(MIN_VALUE)) {
                if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
                    return MIN_VALUE; // recall that -MIN_VALUE == MIN_VALUE
                else if (divisor.eq(MIN_VALUE))
                    return ONE;
                else {
                    // At this point, we have |other| >= 2, so |this/other| < |MIN_VALUE|.
                    const halfThis = this.shr(1);
                    const approx = halfThis.div(divisor).shl(1);
                    if (approx.eq(ZERO)) {
                        return divisor.isNegative() ? ONE : NEG_ONE;
                    }
                    else {
                        rem = this.sub(divisor.mul(approx));
                        res = approx.add(rem.div(divisor));
                        return res;
                    }
                }
            }
            else if (divisor.eq(MIN_VALUE))
                return this.unsigned ? UZERO : ZERO;
            if (this.isNegative()) {
                if (divisor.isNegative())
                    return this.neg().div(divisor.neg());
                return this.neg().div(divisor).neg();
            }
            else if (divisor.isNegative())
                return this.div(divisor.neg()).neg();
            res = ZERO;
        }
        else {
            // The algorithm below has not been made for unsigned longs. It's therefore
            // required to take special care of the MSB prior to running it.
            if (!divisor.unsigned)
                divisor = divisor.toUnsigned();
            if (divisor.gt(this))
                return UZERO;
            if (divisor.gt(this.shru(1)))
                return UONE;
            res = UZERO;
        }
        // Repeat the following until the remainder is less than other:  find a
        // floating-point that approximates remainder / other *from below*, add this
        // into the result, and subtract it from the remainder.  It is critical that
        // the approximate value is less than or equal to the real value so that the
        // remainder never becomes negative.
        rem = this;
        while (rem.gte(divisor)) {
            // Approximate the result of division. This may be a little greater or
            // smaller than the actual value.
            let approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
            // We will tweak the approximate result by changing it in the 48-th digit or
            // the smallest non-fractional digit, whichever is larger.
            // tslint:disable-next-line:prefer-const
            // tslint:disable-next-line:semicolon
            const log2 = Math.ceil(Math.log(approx) / Math.LN2);
            const delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48);
            // Decrease the approximation until it is smaller than the remainder.  Note
            // that if it is too large, the product overflows and is negative.
            let approxRes = fromNumber(approx);
            let approxRem = approxRes.mul(divisor);
            while (approxRem.isNegative() || approxRem.gt(rem)) {
                approx -= delta;
                approxRes = fromNumber(approx, this.unsigned);
                approxRem = approxRes.mul(divisor);
            }
            // We know the answer can't be zero... and actually, zero would cause
            // infinite recursion since we would make no progress.
            if (approxRes.isZero())
                approxRes = ONE;
            res = res.add(approxRes);
            rem = rem.sub(approxRem);
        }
        return res;
    }
    /**
     * Returns this Long modulo the specified.
     * @param {!Long|number|string} divisor Divisor
     * @returns {!Long} Remainder
     */
    modulo(divisor) {
        if (!isLong(divisor))
            divisor = fromValue(divisor);
        return this.sub(this.div(divisor).mul(divisor));
    }
    /**
     * Returns the bitwise NOT of this
     * @returns {!Long}
     */
    not() {
        return fromBits(~this.low, ~this.high, this.unsigned);
    }
    /**
     * Returns the bitwise AND of this Long and the specified.
     * @param {!Long|number|string} other Other Long
     * @returns {!Long}
     */
    and(other) {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low & other.low, this.high & other.high, this.unsigned);
    }
    /**
     * Returns the bitwise OR of this Long and the specified.
     * @param {!Long|number|string} other Other Long
     * @returns {!Long}
     */
    or(other) {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low | other.low, this.high | other.high, this.unsigned);
    }
    /**
     * Returns the bitwise XOR of this Long and the given one.
     * @param {!Long|number|string} other Other Long
     * @returns {!Long}
     */
    xor(other) {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
    }
    /**
     * Returns this Long with bits shifted to the left by the given amount.
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    shiftLeft(numBits) {
        if (isLong(numBits))
            numBits = numBits.toInt();
        numBits = numBits & 63;
        if (numBits === 0)
            return this;
        else if (numBits < 32)
            return fromBits(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
        else
            return fromBits(0, this.low << (numBits - 32), this.unsigned);
    }
    /**
     * Returns this Long with bits arithmetically shifted to the right by the given amount.
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    shiftRight(numBits) {
        if (isLong(numBits))
            numBits = numBits.toInt();
        numBits = numBits & 63;
        if (numBits === 0)
            return this;
        else if (numBits < 32)
            return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
        else
            return fromBits(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
    }
    /**
     * Returns this Long with bits logically shifted to the right by the given amount.
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    shiftRightUnsigned(numBits) {
        if (isLong(numBits))
            numBits = numBits.toInt();
        numBits = numBits & 63;
        if (numBits === 0)
            return this;
        else {
            const high = this.high;
            if (numBits < 32) {
                const low = this.low;
                return fromBits((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
            }
            else if (numBits === 32)
                return fromBits(high, 0, this.unsigned);
            else
                return fromBits(high >>> (numBits - 32), 0, this.unsigned);
        }
    }
    /**
     * Converts this Long to signed.
     * @returns {!Long} Signed long
     */
    toSigned() {
        if (!this.unsigned)
            return this;
        return fromBits(this.low, this.high, false);
    }
    /**
     * Converts this Long to unsigned.
     * @returns {!Long} Unsigned long
     */
    toUnsigned() {
        if (this.unsigned)
            return this;
        return fromBits(this.low, this.high, true);
    }
    /**
     * Converts this Long to its byte representation.
     * @param {boolean=} le Whether little or big endian, defaults to big endian
     * @returns {!Array.<number>} Byte representation
     */
    toBytes(le) {
        return le ? this.toBytesLE() : this.toBytesBE();
    }
    /**
     * Converts this Long to its little endian byte representation.
     * @returns {!Array.<number>} Little endian byte representation
     */
    toBytesLE() {
        const hi = this.high;
        const lo = this.low;
        return [
            lo & 0xff,
            (lo >>> 8) & 0xff,
            (lo >>> 16) & 0xff,
            (lo >>> 24) & 0xff,
            hi & 0xff,
            (hi >>> 8) & 0xff,
            (hi >>> 16) & 0xff,
            (hi >>> 24) & 0xff,
        ];
    }
    /**
     * Converts this Long to its big endian byte representation.
     * @returns {!Array.<number>} Big endian byte representation
     */
    toBytesBE() {
        const hi = this.high;
        const lo = this.low;
        return [
            (hi >>> 24) & 0xff,
            (hi >>> 16) & 0xff,
            (hi >>> 8) & 0xff,
            hi & 0xff,
            (lo >>> 24) & 0xff,
            (lo >>> 16) & 0xff,
            (lo >>> 8) & 0xff,
            lo & 0xff,
        ];
    }
    [_Symbol.reflection]() {
        return {
            type: "System.Int64",
            interfaces: ["FSharpRecord", "System.IComparable"],
            properties: {
                low: "number",
                high: "number",
                unsigned: "boolean",
            },
        };
    }
}
// A cache of the Long representations of small integer values.
const INT_CACHE = {};
// A cache of the Long representations of small unsigned integer values.
const UINT_CACHE = {};
/**
 * Tests if the specified object is a
 * @param {*} obj Object
 * @returns {boolean}
 */
function isLong(obj) {
    return (obj && obj instanceof Long);
}
/**
 * Returns a Long representing the given 32 bit integer value.
 * @param {number} value The 32 bit integer in question
 * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @returns {!Long} The corresponding Long value
 */
function fromInt(value, unsigned = false) {
    let obj;
    let cachedObj;
    let cache = false;
    if (unsigned) {
        value >>>= 0;
        if (0 <= value && value < 256) {
            cache = true;
            cachedObj = UINT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = fromBits(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache)
            UINT_CACHE[value] = obj;
        return obj;
    }
    else {
        value |= 0;
        if (-128 <= value && value < 128) {
            cache = true;
            cachedObj = INT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = fromBits(value, value < 0 ? -1 : 0, false);
        if (cache)
            INT_CACHE[value] = obj;
        return obj;
    }
}
/**
 * Returns a Long representing the given value, provided that it is a finite number. Otherwise, zero is returned.
 * @param {number} value The number in question
 * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @returns {!Long} The corresponding Long value
 */
function fromNumber(value, unsigned = false) {
    if (isNaN(value) || !isFinite(value)) {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    }
    else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return fromNumber(-value, unsigned).neg();
    return fromBits((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}
/**
 * Returns a Long representing the 64 bit integer that comes by concatenating the given low and high bits. Each is
 *  assumed to use 32 bits.
 * @param {number} lowBits The low 32 bits
 * @param {number} highBits The high 32 bits
 * @param {boolean=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @returns {!Long} The corresponding Long value
 */
function fromBits(lowBits, highBits, unsigned) {
    return new Long(lowBits, highBits, unsigned);
}
/**
 * @param {number} base
 * @param {number} exponent
 * @returns {number}
 */
const pow_dbl = Math.pow; // Used 4 times (4*8 to 15+4)
/**
 * Returns a Long representation of the given string, written using the specified radix.
 * @param {string} str The textual representation of the Long
 * @param {(boolean|number)=} unsigned Whether unsigned or not, defaults to `false` for signed
 * @param {number=} radix The radix in which the text is written (2-36), defaults to 10
 * @returns {!Long} The corresponding Long value
 */
function fromString(str, unsigned = false, radix = 10) {
    if (isValid(str, radix) === null) {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
    if (str.length === 0)
        throw Error("empty string");
    if (str === "NaN" || str === "Infinity" || str === "+Infinity" || str === "-Infinity")
        return ZERO;
    if (typeof unsigned === "number") {
        // For goog.math.long compatibility
        radix = unsigned,
            unsigned = false;
    }
    else {
        unsigned = !!unsigned;
    }
    radix = radix || 10;
    if (radix < 2 || 36 < radix)
        throw RangeError("radix");
    const p = str.indexOf("-");
    if (p > 0)
        throw Error("interior hyphen");
    else if (p === 0) {
        return fromString(str.substring(1), unsigned, radix).neg();
    }
    // Do several (8) digits each time through the loop, so as to
    // minimize the calls to the very expensive emulated div.
    const radixToPower = fromNumber(pow_dbl(radix, 8));
    let result = ZERO;
    for (let i = 0; i < str.length; i += 8) {
        const size = Math.min(8, str.length - i);
        const value = parseInt(str.substring(i, i + size), radix);
        if (size < 8) {
            const power = fromNumber(pow_dbl(radix, size));
            result = result.mul(power).add(fromNumber(value));
        }
        else {
            result = result.mul(radixToPower);
            result = result.add(fromNumber(value));
        }
    }
    result.unsigned = unsigned;
    return result;
}
/**
 * Converts the specified value to a
 * @param {!Long|number|string|!{low: number, high: number, unsigned: boolean}} val Value
 * @returns {!Long}
 */
function fromValue(val) {
    if (val /* is compatible */ instanceof Long)
        return val;
    if (typeof val === "number")
        return fromNumber(val);
    if (typeof val === "string")
        return fromString(val);
    // Throws for non-objects, converts non-instanceof Long:
    return fromBits(val.low, val.high, val.unsigned);
}
// NOTE: the compiler should inline these constant values below and then remove these variables, so there should be
// no runtime penalty for these.
const TWO_PWR_16_DBL = 1 << 16;
const TWO_PWR_24_DBL = 1 << 24;
const TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
const TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
const TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
const TWO_PWR_24 = fromInt(TWO_PWR_24_DBL);
/**
 * Signed zero.
 * @type {!Long}
 */
const ZERO = fromInt(0);
/**
 * Unsigned zero.
 * @type {!Long}
 */
const UZERO = fromInt(0, true);
/**
 * Signed one.
 * @type {!Long}
 */
const ONE = fromInt(1);
/**
 * Unsigned one.
 * @type {!Long}
 */
const UONE = fromInt(1, true);
/**
 * Signed negative one.
 * @type {!Long}
 */
const NEG_ONE = fromInt(-1);
/**
 * Maximum signed value.
 * @type {!Long}
 */
const MAX_VALUE = fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0, false);
/**
 * Maximum unsigned value.
 * @type {!Long}
 */
const MAX_UNSIGNED_VALUE = fromBits(0xFFFFFFFF | 0, 0xFFFFFFFF | 0, true);
/**
 * Minimum signed value.
 * @type {!Long}
 */
const MIN_VALUE = fromBits(0, 0x80000000 | 0, false);

function create$2(d = 0, h = 0, m = 0, s = 0, ms = 0) {
    switch (arguments.length) {
        case 1:
            // ticks
            return fromTicks(arguments[0]);
        case 3:
            // h,m,s
            d = 0, h = arguments[0], m = arguments[1], s = arguments[2], ms = 0;
            break;
        default:
            // d,h,m,s,ms
            break;
    }
    return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
}
function fromTicks(ticks) {
    return ticks.div(10000).toNumber();
}

/* tslint:disable */
function parse$1(v, kind) {
    if (kind == null) {
        kind = typeof v === "string" && v.slice(-1) === "Z" ? 1 /* UTC */ : 2 /* Local */;
    }
    let date = (v == null) ? new Date() : new Date(v);
    if (isNaN(date.getTime())) {
        // Check if this is a time-only string, which JS Date parsing cannot handle (see #1045)
        if (typeof v === "string" && /^(?:[01]?\d|2[0-3]):(?:[0-5]?\d)(?::[0-5]?\d(?:\.\d+)?)?(?:\s*[AaPp][Mm])?$/.test(v)) {
            const d = new Date();
            date = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate() + " " + v);
        }
        else {
            throw new Error("The string is not a valid Date.");
        }
    }
    if (kind === 2 /* Local */) {
        date.kind = kind;
    }
    return date;
}
/* tslint:enable */

function create$1(year, month, day, h = 0, m = 0, s = 0, ms = 0, kind = 2 /* Local */) {
    let date;
    if (kind === 2 /* Local */) {
        date = new Date(year, month - 1, day, h, m, s, ms);
        date.kind = kind;
    }
    else {
        date = new Date(Date.UTC(year, month - 1, day, h, m, s, ms));
    }
    if (isNaN(date.getTime())) {
        throw new Error("The parameters describe an unrepresentable Date.");
    }
    return date;
}
function now() {
    return parse$1();
}

















function ticks(d) {
    return fromNumber(d.getTime())
        .add(62135596800000) // UnixEpochMilliseconds
        .sub(d.kind === 2 /* Local */ ? d.getTimezoneOffset() * 60 * 1000 : 0)
        .mul(10000);
}






function addSeconds(d, v) {
    return parse$1(d.getTime() + v * 1000, d.kind || 1 /* UTC */);
}

// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex

const fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;




function toHex(value) {
    return value < 0
        ? "ff" + (16777215 - (Math.abs(value) - 1)).toString(16)
        : value.toString(16);
}
function fsFormat(str, ...args) {
    function formatOnce(str2, rep) {
        return str2.replace(fsFormatRegExp, (_, prefix, flags, pad, precision, format) => {
            switch (format) {
                case "f":
                case "F":
                    rep = rep.toFixed(precision || 6);
                    break;
                case "g":
                case "G":
                    rep = rep.toPrecision(precision);
                    break;
                case "e":
                case "E":
                    rep = rep.toExponential(precision);
                    break;
                case "O":
                    rep = toString(rep);
                    break;
                case "A":
                    rep = toString(rep, true);
                    break;
                case "x":
                    rep = toHex(Number(rep));
                    break;
                case "X":
                    rep = toHex(Number(rep)).toUpperCase();
                    break;
            }
            const plusPrefix = flags.indexOf("+") >= 0 && parseInt(rep, 10) >= 0;
            pad = parseInt(pad, 10);
            if (!isNaN(pad)) {
                const ch = pad >= 0 && flags.indexOf("0") >= 0 ? "0" : " ";
                rep = padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
            }
            const once = prefix + (plusPrefix ? "+" + rep : rep);
            return once.replace(/%/g, "%%");
        });
    }
    if (args.length === 0) {
        return (cont) => {
            if (fsFormatRegExp.test(str)) {
                return (...args2) => {
                    let strCopy = str;
                    for (const arg of args2) {
                        strCopy = formatOnce(strCopy, arg);
                    }
                    return cont(strCopy.replace(/%%/g, "%"));
                };
            }
            else {
                return cont(str);
            }
        };
    }
    else {
        for (const arg of args) {
            str = formatOnce(str, arg);
        }
        return str.replace(/%%/g, "%");
    }
}






function join(delimiter, xs) {
    let xs2 = xs;
    const len = arguments.length;
    if (len > 2) {
        xs2 = Array(len - 1);
        for (let key = 1; key < len; key++) {
            xs2[key - 1] = arguments[key];
        }
    }
    else if (!Array.isArray(xs)) {
        xs2 = Array.from(xs);
    }
    return xs2.map((x) => toString(x)).join(delimiter);
}

function padLeft(str, len, ch, isRight) {
    ch = ch || " ";
    str = String(str);
    len = len - str.length;
    for (let i = 0; i < len; i++) {
        str = isRight ? str + ch : ch + str;
    }
    return str;
}

class Result {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }
    Equals(other) {
        return equalsUnions(this, other);
    }
    CompareTo(other) {
        return compareUnions(this, other);
    }
    [_Symbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Core.FSharpResult",
            interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
            cases: [["Ok", Any], ["Error", Any]],
        };
    }
}
function map$4(f, result) {
    return result.tag === 0 ? new Result(0, f(result.data)) : result;
}
function mapError(f, result) {
    return result.tag === 1 ? new Result(1, f(result.data)) : result;
}
function bind(f, result) {
    return result.tag === 0 ? f(result.data) : result;
}

function op_EqualsGreater(x, y) {
  return [x, y];
}


function flip(func, x, y) {
  return func(y, x);
}

const AsyncResultLog = function (__exports) {
  const log = __exports.log = function (tag, result) {
    return ofArray$1([[tag, {
      formatFn: fsFormat("%A"),
      input: "%A"
    }.formatFn(x => x)(result)]]);
  };

  const andThen = __exports.andThen = function (func, asyncResultLog) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(asyncResultLog, function (_arg1) {
          return builder_.Bind(_arg1[0].tag === 0 ? function (builder__1) {
            return builder__1.Delay(function () {
              return builder__1.Bind(func(_arg1[0].data), function (_arg2) {
                const resultLog = [_arg2[0], append$1(_arg1[1], append$1(_arg2[1], log("and then", _arg2[0])))];
                return builder__1.Return(resultLog);
              });
            });
          }(singleton) : function (arg00) {
            return singleton.Return(arg00);
          }([new Result(1, _arg1[0].data), _arg1[1]]), function (_arg3) {
            return builder_.Return(_arg3);
          });
        });
      });
    }(singleton);
  };

  const resultLog = __exports.resultLog = function (tag, result) {
    return [result, log(tag, result)];
  };

  const singleton$$1 = __exports.singleton = function (tag, x) {
    const result = new Result(0, x);
    return function (arg00) {
      return singleton.Return(arg00);
    }(resultLog(tag, result));
  };

  const ComputationExpression = __exports.ComputationExpression = class ComputationExpression {
    [_Symbol.reflection]() {
      return {
        type: "Fable.EdIlyin.Core.AsyncResultLog.ComputationExpression",
        properties: {}
      };
    }

    constructor() {}

    Bind(m, f) {
      return andThen(f, m);
    }

    Return(x) {
      return singleton$$1("return", x);
    }

    ReturnFrom(m) {
      return m;
    }

    Zero() {
      return (arg00 => singleton.Return(arg00))(resultLog("zero", new Result(0, null)));
    }

  };
  setType("Fable.EdIlyin.Core.AsyncResultLog.ComputationExpression", ComputationExpression);

  const fromAsyncResult = __exports.fromAsyncResult = function (tag, asyncResult) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(asyncResult, function (_arg1) {
          return builder_.Bind(function (arg00) {
            return singleton.Return(arg00);
          }(resultLog(tag, _arg1)), function (_arg2) {
            return builder_.Return(_arg2);
          });
        });
      });
    }(singleton);
  };

  const print = __exports.print = function (maxChars, asyncResultLog) {
    const truncate$$1 = function (s) {
      if (s.length <= maxChars) {
        return s;
      } else {
        return flip(function (x, y) {
          return x + y;
        }, "...", join("", truncate(maxChars - 3, s)));
      }
    };

    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(asyncResultLog, function (_arg1) {
          iterate(function (tupledArg) {
            ($var1 => ({
              formatFn: fsFormat("%s: %s"),
              input: "%s: %s"
            }).formatFn(x => {
              console.log(x);
            })(tupledArg[0], $var1))(truncate$$1(tupledArg[1]));
          }, _arg1[1]);
          return builder_.Zero();
        });
      });
    }(singleton);
  };

  const fromPromise = __exports.fromPromise = function (tag, promise) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(awaitPromise(promise), function (_arg1) {
          return builder_.ReturnFrom(singleton$$1(tag, _arg1));
        });
      });
    }(singleton);
  };

  const fromResultAsyncResult = __exports.fromResultAsyncResult = function (tag, resultAsyncResult) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(resultAsyncResult.tag === 0 ? resultAsyncResult.data : function (arg00) {
          return singleton.Return(arg00);
        }(new Result(1, resultAsyncResult.data)), function (_arg1) {
          return builder_.Return(resultLog(tag, _arg1));
        });
      });
    }(singleton);
  };

  const fromPromiseResult = __exports.fromPromiseResult = function (tag, promiseResult) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(awaitPromise(promiseResult), function (_arg1) {
          return builder_.Return(resultLog(tag, _arg1));
        });
      });
    }(singleton);
  };

  const mapError$$1 = __exports.mapError = function (func, asyncResultLog) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(asyncResultLog, function (_arg1) {
          const response = [mapError(func, _arg1[0]), _arg1[1]];
          return builder_.Return(response);
        });
      });
    }(singleton);
  };

  const fromResult = __exports.fromResult = function (tag, result) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Return(resultLog(tag, result));
      });
    }(singleton);
  };

  const _catch = __exports.catch = function (asyncResultLog) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(catchAsync(asyncResultLog), function (_arg1) {
          let response;

          if (_arg1.tag === 0) {
            const copyOfStruct = _arg1.data[0];

            if (copyOfStruct.tag === 0) {
              response = [new Result(0, copyOfStruct.data), append$1(_arg1.data[1], log("catch", new Result(0, copyOfStruct.data)))];
            } else {
              response = [new Result(1, copyOfStruct.data), append$1(_arg1.data[1], log("catch", new Result(1, copyOfStruct.data)))];
            }
          } else {
            response = resultLog("catch", new Result(1, _arg1.data.message));
          }

          return builder_.Return(response);
        });
      });
    }(singleton);
  };

  const resultLogMap2 = __exports.resultLogMap2 = function (func, rl1_0, rl1_1, rl2_0, rl2_1) {
    const rl1 = [rl1_0, rl1_1];
    const rl2 = [rl2_0, rl2_1];
    const l = append$1(rl1[1], rl2[1]);
    const matchValue = [rl1[0], rl2[0]];

    if (matchValue[0].tag === 0) {
      if (matchValue[1].tag === 0) {
        return op_EqualsGreater(new Result(0, func(matchValue[0].data, matchValue[1].data)), l);
      } else {
        return [new Result(1, matchValue[1].data), l];
      }
    } else {
      return [new Result(1, matchValue[0].data), l];
    }
  };

  const conCollect = __exports.conCollect = function (asyncResultLogList) {
    return function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(parallel(asyncResultLogList), function (_arg1) {
          const response = function (list) {
            return (() => {
              let folder;

              const func = function (e, l) {
                return new List(e, l);
              };

              folder = function (tupledArg, tupledArg_1) {
                return resultLogMap2(func, tupledArg[0], tupledArg[1], tupledArg_1[0], tupledArg_1[1]);
              };

              return function (state) {
                return foldBack(folder, list, state);
              };
            })()(op_EqualsGreater(new Result(0, new List()), new List()));
          }(ofArray$1(_arg1));

          return builder_.Return(response);
        });
      });
    }(singleton);
  };

  return __exports;
}({});
const AsyncResultLogAutoOpen = function (__exports) {
  const asyncResultLog = __exports.asyncResultLog = new AsyncResultLog.ComputationExpression();
  return __exports;
}({});

const _Promise = function (__exports) {
  const result = __exports.result = function (a) {
    return a.then($var1 => new Result(0, $var1), $var2 => new Result(1, $var2));
  };

  const mapResult = __exports.mapResult = function (fn, a) {
    return a.then(function (result_1) {
      return map$4(fn, result_1);
    });
  };

  const bindResult = __exports.bindResult = function (fn, a) {
    return a.then(function (a_1) {
      return a_1.tag === 1 ? Promise.resolve(new Result(1, a_1.data)) : result(fn(a_1.data));
    });
  };

  const PromiseBuilder = __exports.PromiseBuilder = class PromiseBuilder {
    [_Symbol.reflection]() {
      return {
        type: "Fable.PowerPack.Promise.PromiseBuilder",
        properties: {}
      };
    }

    constructor() {}

    For(seq, body) {
      let p = Promise.resolve(null);

      for (let a of seq) {
        p = p.then(() => body(a));
      }

      return p;
    }

    While(guard, p) {
      if (guard()) {
        return p.then(() => this.While(guard, p));
      } else {
        return Promise.resolve(null);
      }
    }

    TryFinally(p, compensation) {
      return p.then(x => {
        compensation();
        return x;
      }, er => {
        compensation();
        throw er;
      });
    }

    Delay(generator) {
      return {
        then: (f1, f2) => {
          try {
            return generator().then(f1, f2);
          } catch (er) {
            if (f2 == null) {
              return Promise.reject(er);
            } else {
              try {
                return Promise.resolve(f2(er));
              } catch (er_1) {
                return Promise.reject(er_1);
              }
            }
          }
        },
        catch: f => {
          try {
            return generator().catch(f);
          } catch (er_2) {
            try {
              return Promise.resolve(f(er_2));
            } catch (er_3) {
              return Promise.reject(er_3);
            }
          }
        }
      };
    }

    Using(resource, binder) {
      return this.TryFinally(binder(resource), () => {
        resource.Dispose();
      });
    }

  };
  setType("Fable.PowerPack.Promise.PromiseBuilder", PromiseBuilder);
  return __exports;
}({});

const PromiseImpl = function (__exports) {
  const promise = __exports.promise = new _Promise.PromiseBuilder();
  return __exports;
}({});

class DecodeResult {
  constructor(tag, data) {
    this.tag = tag;
    this.data = data;
  }

  [_Symbol.reflection]() {
    return {
      type: "Fable.EdIlyin.Core.Decode.DecodeResult",
      interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
      cases: [["Decoded", GenericParam("T")], ["ExpectingButGot", "string", "string"], ["ErrorMessage", "string"]]
    };
  }

  Equals(other) {
    return this === other || this.tag === other.tag && equals(this.data, other.data);
  }

  CompareTo(other) {
    return compareUnions(this, other) | 0;
  }

}
setType("Fable.EdIlyin.Core.Decode.DecodeResult", DecodeResult);
class Decoder {
  constructor(decoder, label) {
    this.decoder = decoder;
    this.label = label;
  }

  [_Symbol.reflection]() {
    return {
      type: "Fable.EdIlyin.Core.Decode.Decoder",
      interfaces: ["FSharpRecord"],
      properties: {
        decoder: FableFunction([GenericParam("From"), makeGeneric(DecodeResult, {
          T: GenericParam("To")
        })]),
        label: "string"
      }
    };
  }

}
setType("Fable.EdIlyin.Core.Decode.Decoder", Decoder);
function run(decoder, jsonValue) {
  return decoder.decoder(jsonValue);
}
function decode(decoder, source) {
  return function (_arg1) {
    return _arg1.tag === 1 ? new Result(1, {
      formatFn: fsFormat("Expecting %s, but instead got: %A"),
      input: "Expecting %s, but instead got: %A"
    }.formatFn(x => x)(_arg1.data[0], _arg1.data[1])) : _arg1.tag === 2 ? new Result(1, _arg1.data) : new Result(0, _arg1.data);
  }(run(decoder, source));
}
function primitive(label, func) {
  return new Decoder(func, label);
}

function succeed(value) {
  return ($var1 => function (label, func) {
    return primitive(label, func);
  }({
    formatFn: fsFormat("%A"),
    input: "%A"
  }.formatFn(x => x)(value), $var1))(function (_arg1) {
    return new DecodeResult(0, value);
  });
}

function getLabel(decoder) {
  return decoder.label;
}
function setLabel(label, decoder) {
  return new Decoder(decoder.decoder, label);
}
function op_LessQmarkGreater(decoder, label) {
  return setLabel(label, decoder);
}
function andThen(func, decoder) {
  const label = getLabel(decoder);
  return primitive(label, function (input) {
    const matchValue = run(decoder, input);

    if (matchValue.tag === 1) {
      return function (tupledArg) {
        return new DecodeResult(1, [tupledArg[0], tupledArg[1]]);
      }(op_EqualsGreater(matchValue.data[0], matchValue.data[1]));
    } else if (matchValue.tag === 2) {
      return new DecodeResult(2, matchValue.data);
    } else {
      return run(func(matchValue.data), input);
    }
  });
}
function op_GreaterGreaterEquals(decoder, func) {
  return andThen(func, decoder);
}
function andMap(decoder, functionDecoder) {
  return op_GreaterGreaterEquals(functionDecoder, function (func) {
    return op_GreaterGreaterEquals(decoder, $var2 => function (value) {
      return succeed(value);
    }(func($var2)));
  });
}
function op_LessMultiplyGreater(fnDecoder, decoder) {
  return andMap(decoder, fnDecoder);
}
function map$5(func, decoder) {
  return op_LessQmarkGreater(op_LessMultiplyGreater(succeed(func), decoder), {
    formatFn: fsFormat("{ %s }"),
    input: "{ %s }"
  }.formatFn(x => x)(getLabel(decoder)));
}

// TODO: This needs improvement, check namespace for non-custom types?

// tslint:disable:max-line-length
// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies

// tslint:disable:ban-types

const Fetch_types = function (__exports) {
  const HttpRequestHeaders = __exports.HttpRequestHeaders = class HttpRequestHeaders {
    constructor(tag, data) {
      this.tag = tag;
      this.data = data;
    }

    [_Symbol.reflection]() {
      return {
        type: "Fable.PowerPack.Fetch.Fetch_types.HttpRequestHeaders",
        interfaces: ["FSharpUnion", "System.IEquatable"],
        cases: [["Accept", "string"], ["Accept-Charset", "string"], ["Accept-Encoding", "string"], ["Accept-Language", "string"], ["Accept-Datetime", "string"], ["Authorization", "string"], ["Cache-Control", "string"], ["Connection", "string"], ["Cookie", "string"], ["Content-Length", "string"], ["Content-MD5", "string"], ["Content-Type", "string"], ["Date", "string"], ["Expect", "string"], ["Forwarded", "string"], ["From", "string"], ["Host", "string"], ["If-Match", "string"], ["If-Modified-Since", "string"], ["If-None-Match", "string"], ["If-Range", "string"], ["If-Unmodified-Since", "string"], ["Max-Forwards", "number"], ["Origin", "string"], ["Pragma", "string"], ["Proxy-Authorization", "string"], ["Range", "string"], ["Referer", "string"], ["SOAPAction", "string"], ["TE", "string"], ["User-Agent", "string"], ["Upgrade", "string"], ["Via", "string"], ["Warning", "string"], ["X-Requested-With", "string"], ["DNT", "string"], ["X-Forwarded-For", "string"], ["X-Forwarded-Host", "string"], ["X-Forwarded-Proto", "string"], ["Front-End-Https", "string"], ["X-Http-Method-Override", "string"], ["X-ATT-DeviceId", "string"], ["X-Wap-Profile", "string"], ["Proxy-Connection", "string"], ["X-UIDH", "string"], ["X-Csrf-Token", "string"], ["Custom", "string", Any]]
      };
    }

    Equals(other) {
      return this === other || this.tag === other.tag && equals(this.data, other.data);
    }

  };
  setType("Fable.PowerPack.Fetch.Fetch_types.HttpRequestHeaders", HttpRequestHeaders);
  const RequestProperties = __exports.RequestProperties = class RequestProperties {
    constructor(tag, data) {
      this.tag = tag;
      this.data = data;
    }

    [_Symbol.reflection]() {
      return {
        type: "Fable.PowerPack.Fetch.Fetch_types.RequestProperties",
        interfaces: ["FSharpUnion", "System.IEquatable"],
        cases: [["Method", "string"], ["Headers", Interface("Fable.PowerPack.Fetch.Fetch_types.IHttpRequestHeaders")], ["Body", Any], ["Mode", "string"], ["Credentials", "string"], ["Cache", "string"]]
      };
    }

    Equals(other) {
      return this === other || this.tag === other.tag && equals(this.data, other.data);
    }

  };
  setType("Fable.PowerPack.Fetch.Fetch_types.RequestProperties", RequestProperties);
  return __exports;
}({});

function object(fields) {
  return createObj(fields, 0);
}
function string(x) {
  return x;
}

const Result$1 = function (__exports) {
  const map2$$1 = __exports.map2 = function (fn, a, b) {
    const matchValue = [a, b];

    if (matchValue[0].tag === 1) {
      return new Result(1, matchValue[0].data);
    } else if (matchValue[1].tag === 1) {
      return new Result(1, matchValue[1].data);
    } else {
      return new Result(0, fn(matchValue[0].data, matchValue[1].data));
    }
  };

  const combineList = __exports.combineList = function (list) {
    return (() => {
      let folder;

      const fn = function (e, l) {
        return new List(e, l);
      };

      folder = function (a, b) {
        return map2$$1(fn, a, b);
      };

      return function (state) {
        return foldBack(folder, list, state);
      };
    })()(new Result(0, new List()));
  };

  const combineArray = __exports.combineArray = function (array) {
    return fold((() => {
      const fn = function (a, e) {
        return function (array2) {
          return a.concat(array2);
        }(Array.from(singleton$1(e)));
      };

      return function (a_1, b) {
        return map2$$1(fn, a_1, b);
      };
    })(), new Result(0, new Array(0)), array);
  };

  const ofOption = __exports.ofOption = function (error, option) {
    if (option != null) {
      return new Result(0, option);
    } else {
      return new Result(1, error);
    }
  };

  const ofChoice = __exports.ofChoice = function (choice) {
    if (choice.tag === 1) {
      return new Result(1, choice.data);
    } else {
      return new Result(0, choice.data);
    }
  };

  const fromResultResult = __exports.fromResultResult = function (resultResult) {
    const $var1 = resultResult.tag === 1 ? [1, resultResult.data] : resultResult.data.tag === 1 ? [1, resultResult.data.data] : [0, resultResult.data.data];

    switch ($var1[0]) {
      case 0:
        return new Result(0, $var1[1]);

      case 1:
        return new Result(1, $var1[1]);
    }
  };

  const andThen = __exports.andThen = function () {
    return function (binder, result) {
      return bind(binder, result);
    };
  };

  const Builder = __exports.Builder = class Builder {
    [_Symbol.reflection]() {
      return {
        type: "Fable.EdIlyin.Core.Result.Builder",
        properties: {}
      };
    }

    constructor() {}

    Bind(m, f) {
      return bind(f, m);
    }

    Return(x) {
      return new Result(0, x);
    }

    ReturnFrom(m) {
      return m;
    }

    Zero() {
      return new Result(0, null);
    }

  };
  setType("Fable.EdIlyin.Core.Result.Builder", Builder);
  return __exports;
}({});
const ResultAutoOpen = function (__exports) {
  const result = __exports.result = new Result$1.Builder();
  return __exports;
}({});

function decodeValue(decoder, jsonValue) {
  return decode(decoder, jsonValue);
}

const value = primitive("a POJO", function (arg0) {
  return new DecodeResult(0, arg0);
});
function field(name, decoder) {
  const label = {
    formatFn: fsFormat("%s field '%s'"),
    input: "%s field '%s'"
  }.formatFn(x => x)(getLabel(decoder), name);
  return primitive(label, function (o) {
    const matchValue = Object.prototype.hasOwnProperty.call(o, name);

    if (matchValue) {
      return function (jsonValue) {
        return run(decoder, jsonValue);
      }(o[name]);
    } else {
      return new DecodeResult(1, [label, {
        formatFn: fsFormat("%A"),
        input: "%A"
      }.formatFn(x => x)(o)]);
    }
  });
}

const bool$1 = primitive("a Bool", function (o) {
  return typeof o === "boolean" ? new DecodeResult(0, o) : function (tupledArg) {
    return new DecodeResult(1, [tupledArg[0], tupledArg[1]]);
  }(op_EqualsGreater("a Bool", {
    formatFn: fsFormat("%A"),
    input: "%A"
  }.formatFn(x => x)(o)));
});

const _int$1 = primitive("an Int", function (o) {
  return (value => {
    if (typeof value !== "number") {
      return false;
    }

    

    if (-2147483648 <= value && value <= 2147483647 && (value | 0) === value) {
      return true;
    }

    if (isFinite(value) && !(value % 1)) {
      return true;
    }

    
    return false;
  })(o) ? new DecodeResult(0, o) : function (tupledArg) {
    return new DecodeResult(1, [tupledArg[0], tupledArg[1]]);
  }(op_EqualsGreater("an Int", {
    formatFn: fsFormat("%A"),
    input: "%A"
  }.formatFn(x => x)(o)));
});

const int64$1 = primitive("an Int64", function (o) {
  return (value => {
    if (typeof value !== "number") {
      return false;
    }

    

    if (-9223372036854775808 <= value && value <= 9223372032559808512 && (value | 0) === value) {
      return true;
    }

    if (isFinite(value) && !(value % 1)) {
      return true;
    }

    
    return false;
  })(o) ? new DecodeResult(0, o) : function (tupledArg) {
    return new DecodeResult(1, [tupledArg[0], tupledArg[1]]);
  }(op_EqualsGreater("an Int64", {
    formatFn: fsFormat("%A"),
    input: "%A"
  }.formatFn(x => x)(o)));
});

const _float$1 = primitive("a Float", function (o) {
  return typeof o === "number" ? new DecodeResult(0, o) : function (tupledArg) {
    return new DecodeResult(1, [tupledArg[0], tupledArg[1]]);
  }(op_EqualsGreater("a Float", {
    formatFn: fsFormat("%A"),
    input: "%A"
  }.formatFn(x => x)(o)));
});


const dateTime = map$5(function (i) {
  const start = create$1(1970, 1, 1, 0, 0, 0, 0, 1);
  return addSeconds(start, i);
}, _float$1);
const string$1 = primitive("a String", function (o) {
  return typeof o === "string" ? new DecodeResult(0, o) : function (tupledArg) {
    return new DecodeResult(1, [tupledArg[0], tupledArg[1]]);
  }(op_EqualsGreater("a String", {
    formatFn: fsFormat("%A"),
    input: "%A"
  }.formatFn(x => x)(o)));
});

es6Promise.polyfill();

function _fetch(url, properties, decoder) {
  return AsyncResultLog.catch(function (builder_) {
    return builder_.Bind(AsyncResultLog.mapError(function (e) {
      return e.message;
    }, AsyncResultLog.fromPromiseResult("try fetch", _Promise.result(fetch(url, createObj(properties, 1))))), function (_arg1) {
      return builder_.Bind(AsyncResultLog.fromResultAsyncResult("decode", decode(decoder, _arg1)), function (_arg2) {
        return builder_.Return(_arg2);
      });
    });
  }(AsyncResultLogAutoOpen.asyncResultLog));
}

function get(url, headers, decoder) {
  const properties = ofArray$1([new Fetch_types.RequestProperties(0, "GET"), new Fetch_types.RequestProperties(1, createObj(headers, 0))]);
  return _fetch(url, properties, decoder);
}


const text = primitive("a Text", function (response) {
  return new DecodeResult(0, function (builder_) {
    return builder_.Delay(function () {
      return builder_.Bind(catchAsync(awaitPromise(response.text())), function (_arg1) {
        const result = mapError(function (e) {
          return e.message;
        }, Result$1.ofChoice(_arg1));
        return builder_.Return(result);
      });
    });
  }(singleton));
});
function json(decoder) {
  return primitive("an JSON", function (response) {
    return new DecodeResult(0, function (builder_) {
      return builder_.Delay(function () {
        return builder_.Bind(awaitPromise(response.json()), function (_arg1) {
          const result = decodeValue(decoder, _arg1);
          return builder_.Return(result);
        });
      });
    }(singleton));
  });
}
const response = primitive("an HTTP response", $var2 => function (arg0_1) {
  return new DecodeResult(0, arg0_1);
}(($var1 => function (arg00) {
  return singleton.Return(arg00);
}(function (arg0) {
  return new Result(0, arg0);
}($var1)))($var2)));

function equal(expected, actual) {
  const assert_ = assert;
  assert_.deepStrictEqual(actual, expected);
}
it("fetch: json echo", function () {
  return function (arg00) {
    return startAsPromise(arg00);
  }(function (builder_) {
    return builder_.Delay(function () {
      return builder_.Bind(get("http://echo.jsontest.com/abba/babba", new List(), json(value)), function (_arg1) {
        const result = equal(new Result(0, object(ofArray$1([["abba", string("babba")]]))), _arg1[0]);
        return builder_.Return();
      });
    });
  }(singleton));
});
it("fetch: wrong address", function () {
  return function (arg00_1) {
    return startAsPromise(arg00_1);
  }(function (builder__1) {
    return builder__1.Delay(function () {
      return builder__1.Bind(get("http://echoa.jsontest.com", new List(), text), function (_arg1_1) {
        const result_1 = equal(new Result(1, "request to http://echoa.jsontest.com failed, reason: getaddrinfo ENOTFOUND echoa.jsontest.com echoa.jsontest.com:80"), _arg1_1[0]);
        return builder__1.Return();
      });
    });
  }(singleton));
});
it("fetch: json echo with decoder", function () {
  return function (arg00_2) {
    return startAsPromise(arg00_2);
  }(function (builder__2) {
    return builder__2.Delay(function () {
      return builder__2.Bind(get("http://echo.jsontest.com/abba/babba", new List(), json(field("abba", string$1))), function (_arg1_2) {
        const result_2 = equal(new Result(0, "babba"), _arg1_2[0]);
        return builder__2.Return();
      });
    });
  }(singleton));
});
it("fetch: json echo with decoder: error", function () {
  return function (arg00_3) {
    return startAsPromise(arg00_3);
  }(function (builder__3) {
    return builder__3.Delay(function () {
      return builder__3.Bind(get("http://echo.jsontest.com/abba/babba", new List(), json(field("abbax", string$1))), function (_arg1_3) {
        const result_3 = equal(new Result(1, "Expecting a String field 'abbax', but instead got: \"{\\\"abba\\\":\\\"babba\\\"}\""), _arg1_3[0]);
        return builder__3.Return();
      });
    });
  }(singleton));
});

it("result: computation expression: return", function () {
  const assert_ = assert;
  assert_.deepStrictEqual(function (builder_) {
    return builder_.Bind(new Result(0, 42), function (_arg1) {
      return builder_.Return(_arg1);
    });
  }(ResultAutoOpen.result), new Result(0, 42));
});
it("result: computation expression: return from", function () {
  const assert__1 = assert;
  assert__1.deepStrictEqual(function (builder__1) {
    const r = new Result(0, 42);
    return builder__1.ReturnFrom(r);
  }(ResultAutoOpen.result), new Result(0, 42));
});
it("result: computation expression: zero", function () {
  const assert__2 = assert;
  assert__2.deepStrictEqual(function (builder__2) {
    ({
      formatFn: fsFormat("%i"),
      input: "%i"
    }).formatFn(x => x)(42);
    return builder__2.Zero();
  }(ResultAutoOpen.result), new Result(0, null));
});

class queue {
  constructor(tag, data) {
    this.tag = tag;
    this.data = data;
  }

  [_Symbol.reflection]() {
    return {
      type: "Fable.EdIlyin.Core.Queue.queue",
      interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
      cases: [["Queue", makeGeneric(List, {
        T: GenericParam("a")
      }), makeGeneric(List, {
        T: GenericParam("a")
      })]]
    };
  }

  Equals(other) {
    return this === other || this.tag === other.tag && equals(this.data, other.data);
  }

  CompareTo(other) {
    return compareUnions(this, other) | 0;
  }

}
setType("Fable.EdIlyin.Core.Queue.queue", queue);
function empty$1() {
  return new queue(0, [new List(), new List()]);
}

function push(_arg1, item) {
  return new queue(0, [_arg1.data[0], new List(item, _arg1.data[1])]);
}
function ofList$1(list) {
  return new queue(0, [list, new List()]);
}

function pull(_arg1) {
  pull: while (true) {
    if (_arg1.data[0].tail != null) {
      return [_arg1.data[0].head, new queue(0, [_arg1.data[0].tail, _arg1.data[1]])];
    } else if (_arg1.data[1].tail == null) {
      return null;
    } else {
      _arg1 = ofList$1(reverse$1(_arg1.data[1]));
      continue pull;
    }
  }
}
function length(_arg1) {
  return _arg1.data[0].length + _arg1.data[1].length | 0;
}

function op_GreaterGreaterEquals$1(asyn, func) {
  return singleton.Bind(asyn, func);
}
function map$7(func, asyn) {
  return op_GreaterGreaterEquals$1(asyn, $var1 => function (arg00) {
    return singleton.Return(arg00);
  }(func($var1)));
}
function op_BarGreaterGreater(asyn, func) {
  return map$7(func, asyn);
}

class QueueCell {
    constructor(message) {
        this.value = message;
    }
}
class MailboxQueue {
    add(message) {
        const itCell = new QueueCell(message);
        if (this.firstAndLast) {
            this.firstAndLast[1].next = itCell;
            this.firstAndLast = [this.firstAndLast[0], itCell];
        }
        else {
            this.firstAndLast = [itCell, itCell];
        }
    }
    tryGet() {
        if (this.firstAndLast) {
            const value = this.firstAndLast[0].value;
            if (this.firstAndLast[0].next) {
                this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
            }
            else {
                delete this.firstAndLast;
            }
            return value;
        }
        return void 0;
    }
}
class MailboxProcessor {
    constructor(body, cancellationToken$$1) {
        this.body = body;
        this.cancellationToken = cancellationToken$$1 || defaultCancellationToken;
        this.messages = new MailboxQueue();
    }
    __processEvents() {
        if (this.continuation) {
            const value = this.messages.tryGet();
            if (value) {
                const cont = this.continuation;
                delete this.continuation;
                cont(value);
            }
        }
    }
    start() {
        startImmediate(this.body(this), this.cancellationToken);
    }
    receive() {
        return fromContinuations((conts) => {
            if (this.continuation) {
                throw new Error("Receive can only be called once!");
            }
            this.continuation = conts[0];
            this.__processEvents();
        });
    }
    post(message) {
        this.messages.add(message);
        this.__processEvents();
    }
    postAndAsyncReply(buildMessage) {
        let result;
        let continuation;
        function checkCompletion() {
            if (result && continuation) {
                continuation(result);
            }
        }
        const reply = {
            reply: (res) => {
                result = res;
                checkCompletion();
            },
        };
        this.messages.add(buildMessage(reply));
        this.__processEvents();
        return fromContinuations((conts) => {
            continuation = conts[0];
            checkCompletion();
        });
    }
}
function start$2(body, cancellationToken$$1) {
    const mbox = new MailboxProcessor(body, cancellationToken$$1);
    mbox.start();
    return mbox;
}

class Model {
  constructor(quantity, millisecond$$1, queue$$1) {
    this.quantity = quantity | 0;
    this.millisecond = millisecond$$1 | 0;
    this.queue = queue$$1;
  }

  [_Symbol.reflection]() {
    return {
      type: "Fable.EdIlyin.Core.Throttle.Model",
      interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
      properties: {
        quantity: "number",
        millisecond: "number",
        queue: makeGeneric(queue, {
          a: "number"
        })
      }
    };
  }

  Equals(other) {
    return equalsRecords(this, other);
  }

  CompareTo(other) {
    return compareRecords(this, other) | 0;
  }

}
setType("Fable.EdIlyin.Core.Throttle.Model", Model);
class Msg {
  constructor(tag, data) {
    this.tag = tag;
    this.data = data;
  }

  [_Symbol.reflection]() {
    return {
      type: "Fable.EdIlyin.Core.Throttle.Msg",
      interfaces: ["FSharpUnion"],
      cases: [["Die"], ["Fetch", FableFunction([Unit, GenericParam("a")]), Any]]
    };
  }

}
setType("Fable.EdIlyin.Core.Throttle.Msg", Msg);
function nowMilliseconds() {
  const milliseconds$$1 = create$2((() => {
    let copyOfStruct = now();
    return ticks(copyOfStruct);
  })());
  return milliseconds$$1;
}

function execute(func, channel, model) {
  (function (arg00) {
    channel.reply(arg00);
  })(func());

  const queue$$1 = push(model.queue, nowMilliseconds());
  return new Model(model.quantity, model.millisecond, queue$$1);
}

function _fetch$2(model, func, channel) {
  return map$7(function (model_1) {
    return execute(func, channel, model_1);
  }, (() => {
    const matchValue = length(model.queue) | 0;

    if (matchValue < model.quantity) {
      return singleton.Return(model);
    } else {
      const matchValue_1 = pull(model.queue);

      if (matchValue_1 != null) {
        const was = matchValue_1[0];
        const tail = matchValue_1[1];
        return op_BarGreaterGreater(sleep(model.millisecond - ~~(nowMilliseconds() - was)), function () {
          return new Model(model.quantity, model.millisecond, tail);
        });
      } else {
        return singleton.Return(model);
      }
    }
  })());
}

function body(model, agent) {
  const loop = function (state) {
    return op_GreaterGreaterEquals$1(agent.receive(), function (_arg1) {
      return _arg1.tag === 1 ? op_GreaterGreaterEquals$1(_fetch$2(state, _arg1.data[0], _arg1.data[1]), loop) : singleton.Zero();
    });
  };

  return loop(model);
}

function start$1(quantity, millisecond$$1) {
  return function (arg00) {
    return start$2(arg00);
  }((() => {
    const model = new Model(quantity, millisecond$$1, empty$1());
    return function (agent) {
      return body(model, agent);
    };
  })());
}
function add$4(throttler, func) {
  return throttler.postAndAsyncReply(function (channel) {
    return new Msg(1, [func, channel]);
  });
}

function equal$1(expected, actual) {
  const assert_ = assert;
  assert_.deepStrictEqual(actual, expected);
}
it("throttle: simple function", function () {
  const throttler = start$1(1, 1000);

  const func = function () {
    return 42;
  };

  return function (arg00) {
    return startAsPromise(arg00);
  }(function (builder_) {
    return builder_.Delay(function () {
      return builder_.Bind(add$4(throttler, func), function (_arg1) {
        return builder_.Return(equal$1(42, _arg1));
      });
    });
  }(singleton));
});
it("throttle: async function", function () {
  const throttler_1 = start$1(2, 1000);

  const func_1 = function () {
    return function (builder__1) {
      return builder__1.Delay(function () {
        return builder__1.Return(42);
      });
    }(singleton);
  };

  return function (arg00_1) {
    return startAsPromise(arg00_1);
  }(function (builder__2) {
    return builder__2.Delay(function () {
      return builder__2.Bind(add$4(throttler_1, func_1), function (_arg1_1) {
        return builder__2.Bind(_arg1_1, function (_arg2) {
          return builder__2.Return(equal$1(42, _arg2));
        });
      });
    });
  }(singleton));
});
function multipleFunTest(func_2, unitVar1) {
  const throttler_2 = start$1(3, 100);
  return function (arg00_2) {
    return startAsPromise(arg00_2);
  }(function (builder__3) {
    return builder__3.Delay(function () {
      return builder__3.Bind(parallel(initialize$1(22, function (_arg1_2) {
        return func_2(function (func_3) {
          return add$4(throttler_2, func_3);
        });
      })), function (_arg1_3) {
        const results = map2(function (tupledArg, x) {
          return tupledArg[0] <= x ? x <= tupledArg[1] : false;
        }, (() => {
          const loop = function () {
            return delay(function () {
              return append(singleton$1([0, 0]), delay(function () {
                return append(singleton$1([0, 10]), delay(function () {
                  return append(singleton$1([90, 110]), delay(function () {
                    return loop();
                  }));
                }));
              }));
            });
          };

          return loop();
        })(), map$1(function (tupledArg_1) {
          return ~~(tupledArg_1[1] - tupledArg_1[0]);
        }, Array.from(pairwise(_arg1_3)), new Int32Array(Array.from(pairwise(_arg1_3)).length)));
        equal$1(initialize(22 - 1, function (_arg2_1) {
          return true;
        }), results);
        return builder__3.Zero();
      });
    });
  }(singleton));
}
it("throttle: multiple simple functions", (() => {
  const func_2 = function (throttle) {
    return throttle(function () {
      return nowMilliseconds();
    });
  };

  return function () {
    return multipleFunTest(func_2, null);
  };
})());
it("throttle: multiple async functions", (() => {
  const func_4 = function (throttle_1) {
    const func_3 = function () {
      return function (builder__3) {
        return builder__3.Delay(function () {
          return builder__3.Return(nowMilliseconds());
        });
      }(singleton);
    };

    return function (builder__4) {
      return builder__4.Delay(function () {
        return builder__4.Bind(throttle_1(func_3), function (_arg1_2) {
          return builder__4.ReturnFrom(_arg1_2);
        });
      });
    }(singleton);
  };

  return function () {
    return multipleFunTest(func_4, null);
  };
})());
class DifferentResult {
  constructor(tag, data) {
    this.tag = tag;
    this.data = data;
  }

  [_Symbol.reflection]() {
    return {
      type: "ThrottleTests.DifferentResult",
      interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
      cases: [["Int", "number"], ["String", "string"]]
    };
  }

  Equals(other) {
    return this === other || this.tag === other.tag && equals(this.data, other.data);
  }

  CompareTo(other) {
    return compareUnions(this, other) | 0;
  }

}
setType("ThrottleTests.DifferentResult", DifferentResult);
it("throttle: couple of different functions", function () {
  const throttler_2 = start$1(4, 100);

  const throttle_2 = function (func_5) {
    return add$4(throttler_2, func_5);
  };

  const patternInput = [42, "thirty two"];

  const func1 = function () {
    return patternInput[0] | 0;
  };

  const func2 = function () {
    return patternInput[1];
  };

  return function (arg00_2) {
    return startAsPromise(arg00_2);
  }(function (builder__5) {
    return builder__5.Delay(function () {
      return builder__5.Bind(throttle_2($var1 => function (arg0) {
        return new DifferentResult(0, arg0);
      }(func1($var1))), function (_arg1_3) {
        return builder__5.Bind(throttle_2($var2 => function (arg0_1) {
          return new DifferentResult(1, arg0_1);
        }(func2($var2))), function (_arg2_1) {
          return builder__5.Return(equal$1([new DifferentResult(0, patternInput[0]), new DifferentResult(1, patternInput[1])], [_arg1_3, _arg2_1]));
        });
      });
    });
  }(singleton));
});
