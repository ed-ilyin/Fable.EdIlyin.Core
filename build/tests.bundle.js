'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var assert = require('assert');
var es6Promise = require('es6-promise');
require('isomorphic-fetch');

const types = new Map();
function setType(fullName, cons) {
    types.set(fullName, cons);
}

var FSymbol = {
    reflection: Symbol("reflection"),
};

function padWithZeros(i, length) {
    let str = i.toString(10);
    while (str.length < length) {
        str = "0" + str;
    }
    return str;
}
function offsetToString(offset) {
    const isMinus = offset < 0;
    offset = Math.abs(offset);
    const hours = ~~(offset / 3600000);
    const minutes = (offset % 3600000) / 60000;
    return (isMinus ? "-" : "+") +
        padWithZeros(hours, 2) + ":" +
        padWithZeros(minutes, 2);
}
function toHalfUTCString(date, half) {
    const str = date.toISOString();
    return half === "first"
        ? str.substring(0, str.indexOf("T"))
        : str.substring(str.indexOf("T") + 1, str.length - 1);
}
function toISOString(d, utc) {
    if (utc) {
        return d.toISOString();
    }
    else {
        // JS Date is always local
        const printOffset = d.kind == null ? true : d.kind === 2;
        return padWithZeros(d.getFullYear(), 4) + "-" +
            padWithZeros(d.getMonth() + 1, 2) + "-" +
            padWithZeros(d.getDate(), 2) + "T" +
            padWithZeros(d.getHours(), 2) + ":" +
            padWithZeros(d.getMinutes(), 2) + ":" +
            padWithZeros(d.getSeconds(), 2) + "." +
            padWithZeros(d.getMilliseconds(), 3) +
            (printOffset ? offsetToString(d.getTimezoneOffset() * -60000) : "");
    }
}
function toISOStringWithOffset(dateWithOffset, offset) {
    const str = dateWithOffset.toISOString();
    return str.substring(0, str.length - 1) + offsetToString(offset);
}
function toStringWithCustomFormat(date, format, utc) {
    return format.replace(/(\w)\1*/g, (match) => {
        let rep = match;
        switch (match.substring(0, 1)) {
            case "y":
                const y = utc ? date.getUTCFullYear() : date.getFullYear();
                rep = match.length < 4 ? y % 100 : y;
                break;
            case "M":
                rep = (utc ? date.getUTCMonth() : date.getMonth()) + 1;
                break;
            case "d":
                rep = utc ? date.getUTCDate() : date.getDate();
                break;
            case "H":
                rep = utc ? date.getUTCHours() : date.getHours();
                break;
            case "h":
                const h = utc ? date.getUTCHours() : date.getHours();
                rep = h > 12 ? h % 12 : h;
                break;
            case "m":
                rep = utc ? date.getUTCMinutes() : date.getMinutes();
                break;
            case "s":
                rep = utc ? date.getUTCSeconds() : date.getSeconds();
                break;
        }
        if (rep !== match && rep < 10 && match.length > 1) {
            rep = "0" + rep;
        }
        return rep;
    });
}
function toStringWithOffset(date, format) {
    const d = new Date(date.getTime() + date.offset);
    if (!format) {
        return d.toISOString().replace(/\.\d+/, "").replace(/[A-Z]|\.\d+/g, " ") + offsetToString(date.offset);
    }
    else if (format.length === 1) {
        switch (format) {
            case "D":
            case "d": return toHalfUTCString(d, "first");
            case "T":
            case "t": return toHalfUTCString(d, "second");
            case "O":
            case "o": return toISOStringWithOffset(d, date.offset);
            default: throw new Error("Unrecognized Date print format");
        }
    }
    else {
        return toStringWithCustomFormat(d, format, true);
    }
}
function toStringWithKind(date, format) {
    const utc = date.kind === 1;
    if (!format) {
        return utc ? date.toUTCString() : date.toLocaleString();
    }
    else if (format.length === 1) {
        switch (format) {
            case "D":
            case "d":
                return utc ? toHalfUTCString(date, "first") : date.toLocaleDateString();
            case "T":
            case "t":
                return utc ? toHalfUTCString(date, "second") : date.toLocaleTimeString();
            case "O":
            case "o":
                return toISOString(date, utc);
            default:
                throw new Error("Unrecognized Date print format");
        }
    }
    else {
        return toStringWithCustomFormat(date, format, utc);
    }
}
function toString$1(date, format) {
    return date.offset != null
        ? toStringWithOffset(date, format)
        : toStringWithKind(date, format);
}
function DateTime(value, kind) {
    kind = kind == null ? 0 /* Unspecified */ : kind;
    const d = new Date(value);
    d.kind = kind | 0;
    return d;
}





function offset(date) {
    const date1 = date;
    return typeof date1.offset === "number"
        ? date1.offset
        : (date.kind === 1 /* UTC */
            ? 0 : date.getTimezoneOffset() * -60000);
}
function create(year, month, day, h = 0, m = 0, s = 0, ms = 0, kind) {
    const dateValue = kind === 1 /* UTC */
        ? Date.UTC(year, month - 1, day, h, m, s, ms)
        : new Date(year, month - 1, day, h, m, s, ms).getTime();
    if (isNaN(dateValue)) {
        throw new Error("The parameters describe an unrepresentable Date.");
    }
    const date = DateTime(dateValue, kind);
    if (year <= 99) {
        date.setFullYear(year, month - 1, day);
    }
    return date;
}
function now() {
    return DateTime(Date.now(), 2 /* Local */);
}





















function addSeconds(d, v) {
    return DateTime(d.getTime() + v * 1000, d.kind);
}









function compare$1(x, y) {
    const xtime = x.getTime();
    const ytime = y.getTime();
    return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
}

class NonDeclaredType {
    constructor(kind, definition, generics) {
        this.kind = kind;
        this.definition = definition;
        this.generics = generics;
    }
    Equals(other) {
        if (this.kind === other.kind && this.definition === other.definition) {
            return typeof this.generics === "object"
                // equalsRecords should also work for Type[] (tuples)
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
    else if (typeof obj[FSymbol.reflection] === "function") {
        const interfaces = obj[FSymbol.reflection]().interfaces;
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
    const propertyMap = typeof obj[FSymbol.reflection] === "function" ? obj[FSymbol.reflection]().properties || [] : obj;
    return Object.getOwnPropertyNames(propertyMap);
}

function toString$$1(obj, quoteStrings = false) {
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
    if (obj instanceof Date) {
        return toString$1(obj);
    }
    if (typeof obj.ToString === "function") {
        return obj.ToString();
    }
    if (hasInterface(obj, "FSharpUnion")) {
        const info = obj[FSymbol.reflection]();
        const uci = info.cases[obj.tag];
        switch (uci.length) {
            case 1:
                return uci[0];
            case 2:
                // For simplicity let's always use parens, in .NET they're ommitted in some cases
                return uci[0] + " (" + toString$$1(obj.data, true) + ")";
            default:
                return uci[0] + " (" + obj.data.map((x) => toString$$1(x, true)).join(",") + ")";
        }
    }
    try {
        return JSON.stringify(obj, (k, v) => {
            return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v)
                : v && typeof v.ToString === "function" ? toString$$1(v) : v;
        });
    }
    catch (err) {
        // Fallback for objects with circular references
        return "{" + Object.getOwnPropertyNames(obj).map((k) => k + ": " + String(obj[k])).join(", ") + "}";
    }
}
class ObjectRef {
    static id(o) {
        if (!ObjectRef.idMap.has(o)) {
            ObjectRef.idMap.set(o, ++ObjectRef.count);
        }
        return ObjectRef.idMap.get(o);
    }
}
ObjectRef.idMap = new WeakMap();
ObjectRef.count = 0;

function hash(x) {
    if (typeof x === typeof 1) {
        return x * 2654435761 | 0;
    }
    if (x != null && typeof x.GetHashCode === "function") {
        return x.GetHashCode();
    }
    else {
        const s = toString$$1(x);
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
    else if (typeof x !== "object" || typeof y !== "object") {
        return x === y;
        // Equals override or IEquatable implementation
    }
    else if (typeof x.Equals === "function") {
        return x.Equals(y);
    }
    else if (typeof y.Equals === "function") {
        return y.Equals(x);
    }
    else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) {
        return false;
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
function comparePrimitives(x, y) {
    return x === y ? 0 : (x < y ? -1 : 1);
}
function compare$$1(x, y) {
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
    else if (typeof x !== "object" || typeof y !== "object") {
        return x === y ? 0 : (x < y ? -1 : 1);
        // Some types (see Long.ts) may just implement the function and not the interface
        // else if (hasInterface(x, "System.IComparable"))
    }
    else if (typeof x.CompareTo === "function") {
        return x.CompareTo(y);
    }
    else if (typeof y.CompareTo === "function") {
        return y.CompareTo(x) * -1;
    }
    else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) {
        return -1;
    }
    else if (Array.isArray(x)) {
        if (x.length !== y.length) {
            return x.length < y.length ? -1 : 1;
        }
        for (let i = 0, j = 0; i < x.length; i++) {
            j = compare$$1(x[i], y[i]);
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
        return compare$1(x, y);
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
            const res = compare$$1(x[key], y[key]);
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
        return res !== 0 ? res : compare$$1(x.data, y.data);
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
        if (typeof o[FSymbol.reflection] === "function") {
            return o[FSymbol.reflection]().type === "Microsoft.FSharp.Collections.FSharpList";
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
                if (typeof proto[FSymbol.reflection] === "function") {
                    cases = proto[FSymbol.reflection]().cases;
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









// ICollection.Clear method can be called on IDictionary
// too so we need to make a runtime check (see #1120)

class Result$1 {
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
    [FSymbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Core.FSharpResult",
            interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
            cases: [["Ok", GenericParam("T")], ["Error", GenericParam("TError")]],
        };
    }
}
function map(f, result) {
    return result.tag === 0 ? new Result$1(0, f(result.data)) : result;
}
function mapError(f, result) {
    return result.tag === 1 ? new Result$1(1, f(result.data)) : result;
}
function bind(f, result) {
    return result.tag === 0 ? f(result.data) : result;
}

// This module is split from List.ts to prevent cyclic dependencies
function ofArray(args, base) {
    let acc = base || new List$1();
    for (let i = args.length - 1; i >= 0; i--) {
        acc = new List$1(args[i], acc);
    }
    return acc;
}
class List$1 {
    constructor(head, tail) {
        this.head = head;
        this.tail = tail;
    }
    ToString() {
        return "[" + Array.from(this).map((x) => toString$$1(x)).join("; ") + "]";
    }
    Equals(other) {
        // Optimization if they are referencially equal
        if (this === other) {
            return true;
        }
        else {
            let cur1 = this;
            let cur2 = other;
            while (equals(cur1.head, cur2.head)) {
                cur1 = cur1.tail;
                cur2 = cur2.tail;
                if (cur1 == null) {
                    return cur2 == null;
                }
            }
            return false;
        }
    }
    CompareTo(other) {
        // Optimization if they are referencially equal
        if (this === other) {
            return 0;
        }
        else {
            let cur1 = this;
            let cur2 = other;
            let res = compare$$1(cur1.head, cur2.head);
            while (res === 0) {
                cur1 = cur1.tail;
                cur2 = cur2.tail;
                if (cur1 == null) {
                    return cur2 == null ? 0 : -1;
                }
                res = compare$$1(cur1.head, cur2.head);
            }
            return res;
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
    [FSymbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Collections.FSharpList",
            interfaces: ["System.IEquatable", "System.IComparable"],
        };
    }
}

class Comparer {
    constructor(f) {
        this.Compare = f || compare$$1;
    }
    [FSymbol.reflection]() {
        return { interfaces: ["System.IComparer"] };
    }
}

class Some {
    constructor(value) {
        this.value = value;
    }
    // We don't prefix it with "Some" for consistency with erased options
    ToString() {
        return toString$$1(this.value);
    }
    Equals(other) {
        if (other == null) {
            return false;
        }
        else {
            return equals(this.value, other instanceof Some
                ? other.value : other);
        }
    }
    CompareTo(other) {
        if (other == null) {
            return 1;
        }
        else {
            return compare$$1(this.value, other instanceof Some
                ? other.value : other);
        }
    }
}
function makeSome(x) {
    return x == null || x instanceof Some ? new Some(x) : x;
}
function getValue(x, acceptNull) {
    if (x == null) {
        if (!acceptNull) {
            throw new Error("Option has no value");
        }
        return null;
    }
    else {
        return x instanceof Some ? x.value : x;
    }
}

function map$4(f, source, TargetCons) {
    const target = new TargetCons(source.length);
    for (let i = 0; i < source.length; i++) {
        target[i] = f(source[i]);
    }
    return target;
}

function append$1(xs, ys) {
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





function compareWith(f, xs, ys) {
    const nonZero = tryFind$1((i) => i !== 0, map2((x, y) => f(x, y), xs, ys));
    return nonZero != null ? getValue(nonZero) : count(xs) - count(ys);
}
function delay(f) {
    return {
        [Symbol.iterator]: () => f()[Symbol.iterator](),
    };
}










function fold$1(f, acc, xs) {
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
function foldBack$1(f, xs, acc) {
    const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
    for (let i = arr.length - 1; i >= 0; i--) {
        acc = f(arr[i], acc, i);
    }
    return acc;
}






function initialize$1(n, f) {
    return delay(() => unfold((i) => i < n ? [f(i), i + 1] : null, 0));
}










// A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
function count(xs) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs)
        ? xs.length
        : fold$1((acc, x) => acc + 1, 0, xs);
}
function map$3(f, xs) {
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





function reduce(f, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return xs.reduce(f);
    }
    const iter = xs[Symbol.iterator]();
    let cur = iter.next();
    if (cur.done) {
        throw new Error("Seq was empty");
    }
    let acc = cur.value;
    while (true) {
        cur = iter.next();
        if (cur.done) {
            break;
        }
        acc = f(acc, cur.value);
    }
    return acc;
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








function tryFind$1(f, xs, defaultValue) {
    for (let i = 0, iter = xs[Symbol.iterator]();; i++) {
        const cur = iter.next();
        if (cur.done) {
            break;
        }
        if (f(cur.value, i)) {
            return makeSome(cur.value);
        }
    }
    return defaultValue === void 0 ? null : makeSome(defaultValue);
}









function unfold(f, fst) {
    return {
        [Symbol.iterator]: () => {
            // Capture a copy of the first value in the closure
            // so the sequence is restarted every time, see #1230
            let acc = fst;
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

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies


class MapTree {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }
}
function tree_sizeAux(acc, m) {
    sizeAux: while (true) {
        if (m.tag === 1) {
            return acc + 1 | 0;
        }
        else if (m.tag === 2) {
            acc = tree_sizeAux(acc + 1, m.data[2]);
            m = m.data[3];
            continue sizeAux;
        }
        else {
            return acc | 0;
        }
    }
}
function tree_size(x) {
    return tree_sizeAux(0, x);
}
function tree_empty() {
    return new MapTree(0);
}
function tree_height(_arg1) {
    return _arg1.tag === 1 ? 1 : _arg1.tag === 2 ? _arg1.data[4] : 0;
}
function tree_mk(l, k, v, r) {
    const matchValue = l.tag === 0 ? r.tag === 0 ? 0 : 1 : 1;
    switch (matchValue) {
        case 0:
            return new MapTree(1, [k, v]);
        case 1:
            const hl = tree_height(l) | 0;
            const hr = tree_height(r) | 0;
            const m = (hl < hr ? hr : hl) | 0;
            return new MapTree(2, [k, v, l, r, m + 1]);
    }
    throw new Error("internal error: Map.tree_mk");
}
function tree_rebalance(t1, k, v, t2) {
    const t1h = tree_height(t1);
    const t2h = tree_height(t2);
    if (t2h > t1h + 2) {
        if (t2.tag === 2) {
            if (tree_height(t2.data[2]) > t1h + 1) {
                if (t2.data[2].tag === 2) {
                    return tree_mk(tree_mk(t1, k, v, t2.data[2].data[2]), t2.data[2].data[0], t2.data[2].data[1], tree_mk(t2.data[2].data[3], t2.data[0], t2.data[1], t2.data[3]));
                }
                else {
                    throw new Error("rebalance");
                }
            }
            else {
                return tree_mk(tree_mk(t1, k, v, t2.data[2]), t2.data[0], t2.data[1], t2.data[3]);
            }
        }
        else {
            throw new Error("rebalance");
        }
    }
    else {
        if (t1h > t2h + 2) {
            if (t1.tag === 2) {
                if (tree_height(t1.data[3]) > t2h + 1) {
                    if (t1.data[3].tag === 2) {
                        return tree_mk(tree_mk(t1.data[2], t1.data[0], t1.data[1], t1.data[3].data[2]), t1.data[3].data[0], t1.data[3].data[1], tree_mk(t1.data[3].data[3], k, v, t2));
                    }
                    else {
                        throw new Error("rebalance");
                    }
                }
                else {
                    return tree_mk(t1.data[2], t1.data[0], t1.data[1], tree_mk(t1.data[3], k, v, t2));
                }
            }
            else {
                throw new Error("rebalance");
            }
        }
        else {
            return tree_mk(t1, k, v, t2);
        }
    }
}
function tree_add(comparer, k, v, m) {
    if (m.tag === 1) {
        const c = comparer.Compare(k, m.data[0]);
        if (c < 0) {
            return new MapTree(2, [k, v, new MapTree(0), m, 2]);
        }
        else if (c === 0) {
            return new MapTree(1, [k, v]);
        }
        return new MapTree(2, [k, v, m, new MapTree(0), 2]);
    }
    else if (m.tag === 2) {
        const c = comparer.Compare(k, m.data[0]);
        if (c < 0) {
            return tree_rebalance(tree_add(comparer, k, v, m.data[2]), m.data[0], m.data[1], m.data[3]);
        }
        else if (c === 0) {
            return new MapTree(2, [k, v, m.data[2], m.data[3], m.data[4]]);
        }
        return tree_rebalance(m.data[2], m.data[0], m.data[1], tree_add(comparer, k, v, m.data[3]));
    }
    return new MapTree(1, [k, v]);
}
function tree_find(comparer, k, m) {
    const res = tree_tryFind(comparer, k, m);
    if (res == null) {
        throw new Error("key not found: " + k);
    }
    return getValue(res);
}
function tree_tryFind(comparer, k, m) {
    tryFind: while (true) {
        if (m.tag === 1) {
            const c = comparer.Compare(k, m.data[0]) | 0;
            if (c === 0) {
                return makeSome(m.data[1]);
            }
            else {
                return null;
            }
        }
        else if (m.tag === 2) {
            const c_1 = comparer.Compare(k, m.data[0]) | 0;
            if (c_1 < 0) {
                comparer = comparer;
                k = k;
                m = m.data[2];
                continue tryFind;
            }
            else if (c_1 === 0) {
                return makeSome(m.data[1]);
            }
            else {
                comparer = comparer;
                k = k;
                m = m.data[3];
                continue tryFind;
            }
        }
        else {
            return null;
        }
    }
}
function tree_spliceOutSuccessor(m) {
    if (m.tag === 1) {
        return [m.data[0], m.data[1], new MapTree(0)];
    }
    else if (m.tag === 2) {
        if (m.data[2].tag === 0) {
            return [m.data[0], m.data[1], m.data[3]];
        }
        else {
            const kvl = tree_spliceOutSuccessor(m.data[2]);
            return [kvl[0], kvl[1], tree_mk(kvl[2], m.data[0], m.data[1], m.data[3])];
        }
    }
    throw new Error("internal error: Map.spliceOutSuccessor");
}
function tree_remove(comparer, k, m) {
    if (m.tag === 1) {
        const c = comparer.Compare(k, m.data[0]);
        if (c === 0) {
            return new MapTree(0);
        }
        else {
            return m;
        }
    }
    else if (m.tag === 2) {
        const c = comparer.Compare(k, m.data[0]);
        if (c < 0) {
            return tree_rebalance(tree_remove(comparer, k, m.data[2]), m.data[0], m.data[1], m.data[3]);
        }
        else if (c === 0) {
            if (m.data[2].tag === 0) {
                return m.data[3];
            }
            else {
                if (m.data[3].tag === 0) {
                    return m.data[2];
                }
                else {
                    const input = tree_spliceOutSuccessor(m.data[3]);
                    return tree_mk(m.data[2], input[0], input[1], input[2]);
                }
            }
        }
        else {
            return tree_rebalance(m.data[2], m.data[0], m.data[1], tree_remove(comparer, k, m.data[3]));
        }
    }
    else {
        return tree_empty();
    }
}
function tree_mem(comparer, k, m) {
    mem: while (true) {
        if (m.tag === 1) {
            return comparer.Compare(k, m.data[0]) === 0;
        }
        else if (m.tag === 2) {
            const c = comparer.Compare(k, m.data[0]) | 0;
            if (c < 0) {
                comparer = comparer;
                k = k;
                m = m.data[2];
                continue mem;
            }
            else if (c === 0) {
                return true;
            }
            else {
                comparer = comparer;
                k = k;
                m = m.data[3];
                continue mem;
            }
        }
        else {
            return false;
        }
    }
}
// function tree_foldFromTo(
//     comparer: IComparer<any>, lo: any, hi: any,
//     f: (k:any, v: any, acc: any) => any, m: MapTree, x: any): any {
//   if (m.tag === 1) {
//     var cLoKey = comparer.Compare(lo, m.data[0]);
//     var cKeyHi = comparer.Compare(m.data[0], hi);
//     var x_1 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.data[0], m.data[1], x) : x;
//     return x_1;
//   } else if (m.tag === 2) {
//     var cLoKey = comparer.Compare(lo, m.data[0]);
//     var cKeyHi = comparer.Compare(m.data[0], hi);
//     var x_1 = cLoKey < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.data[2], x) : x;
//     var x_2 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.data[0], m.data[1], x_1) : x_1;
//     var x_3 = cKeyHi < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.data[3], x_2) : x_2;
//     return x_3;
//   }
//   return x;
// }
// function tree_foldSection(
//     comparer: IComparer<any>, lo: any, hi: any,
//     f: (k: any, v: any, acc: any) => any, m: MapTree, x: any) {
//   return comparer.Compare(lo, hi) === 1 ? x : tree_foldFromTo(comparer, lo, hi, f, m, x);
// }
// function tree_loop(m: MapTree, acc: any): List<[any,any]> {
//   return m.tag === 1
//     ? new List([m.data[0], m.data[1]], acc)
//     : m.tag === 2
//       ? tree_loop(m.data[2], new List([m.data[0], m.data[1]], tree_loop(m.data[3], acc)))
//       : acc;
// }
// function tree_toList(m: MapTree) {
//   return tree_loop(m, new List());
// }
// function tree_toArray(m: MapTree) {
//   return Array.from(tree_toList(m));
// }
// function tree_ofList(comparer: IComparer<any>, l: List<[any,any]>) {
//   return Seq.fold((acc: MapTree, tupledArg: [any, any]) => {
//     return tree_add(comparer, tupledArg[0], tupledArg[1], acc);
//   }, tree_empty(), l);
// }
function tree_mkFromEnumerator(comparer, acc, e) {
    let cur = e.next();
    while (!cur.done) {
        acc = tree_add(comparer, cur.value[0], cur.value[1], acc);
        cur = e.next();
    }
    return acc;
}
// function tree_ofArray(comparer: IComparer<any>, arr: ArrayLike<[any,any]>) {
//   var res = tree_empty();
//   for (var i = 0; i <= arr.length - 1; i++) {
//     res = tree_add(comparer, arr[i][0], arr[i][1], res);
//   }
//   return res;
// }
function tree_ofSeq(comparer, c) {
    const ie = c[Symbol.iterator]();
    return tree_mkFromEnumerator(comparer, tree_empty(), ie);
}
// function tree_copyToArray(s: MapTree, arr: ArrayLike<any>, i: number) {
//   tree_iter((x, y) => { arr[i++] = [x, y]; }, s);
// }
function tree_collapseLHS(stack) {
    if (stack.tail != null) {
        if (stack.head.tag === 1) {
            return stack;
        }
        else if (stack.head.tag === 2) {
            return tree_collapseLHS(ofArray([
                stack.head.data[2],
                new MapTree(1, [stack.head.data[0], stack.head.data[1]]),
                stack.head.data[3],
            ], stack.tail));
        }
        else {
            return tree_collapseLHS(stack.tail);
        }
    }
    else {
        return new List$1();
    }
}
function tree_mkIterator(s) {
    return { stack: tree_collapseLHS(new List$1(s, new List$1())), started: false };
}
function tree_moveNext(i) {
    function current(it) {
        if (it.stack.tail == null) {
            return null;
        }
        else if (it.stack.head.tag === 1) {
            return [it.stack.head.data[0], it.stack.head.data[1]];
        }
        throw new Error("Please report error: Map iterator, unexpected stack for current");
    }
    if (i.started) {
        if (i.stack.tail == null) {
            return { done: true, value: null };
        }
        else {
            if (i.stack.head.tag === 1) {
                i.stack = tree_collapseLHS(i.stack.tail);
                return {
                    done: i.stack.tail == null,
                    value: current(i),
                };
            }
            else {
                throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
            }
        }
    }
    else {
        i.started = true;
        return {
            done: i.stack.tail == null,
            value: current(i),
        };
    }
}
class FableMap {
    /** Do not call, use Map.create instead. */
    constructor() { return; }
    ToString() {
        return "map [" + Array.from(this).map((x) => toString$$1(x)).join("; ") + "]";
    }
    Equals(m2) {
        return this.CompareTo(m2) === 0;
    }
    CompareTo(m2) {
        return this === m2 ? 0 : compareWith((kvp1, kvp2) => {
            const c = this.comparer.Compare(kvp1[0], kvp2[0]);
            return c !== 0 ? c : compare$$1(kvp1[1], kvp2[1]);
        }, this, m2);
    }
    [Symbol.iterator]() {
        const i = tree_mkIterator(this.tree);
        return {
            next: () => tree_moveNext(i),
        };
    }
    entries() {
        return this[Symbol.iterator]();
    }
    keys() {
        return map$3((kv) => kv[0], this);
    }
    values() {
        return map$3((kv) => kv[1], this);
    }
    get(k) {
        return tree_find(this.comparer, k, this.tree);
    }
    has(k) {
        return tree_mem(this.comparer, k, this.tree);
    }
    /** Mutating method */
    set(k, v) {
        this.tree = tree_add(this.comparer, k, v, this.tree);
    }
    /** Mutating method */
    delete(k) {
        // TODO: Is calculating the size twice is more performant than calling tree_mem?
        const oldSize = tree_size(this.tree);
        this.tree = tree_remove(this.comparer, k, this.tree);
        return oldSize > tree_size(this.tree);
    }
    /** Mutating method */
    clear() {
        this.tree = tree_empty();
    }
    get size() {
        return tree_size(this.tree);
    }
    [FSymbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Collections.FSharpMap",
            interfaces: ["System.IEquatable", "System.IComparable", "System.Collections.Generic.IDictionary"],
        };
    }
}
function from(comparer, tree) {
    const map$$1 = new FableMap();
    map$$1.tree = tree;
    map$$1.comparer = comparer || new Comparer();
    return map$$1;
}
function create$1(ie, comparer) {
    comparer = comparer || new Comparer();
    return from(comparer, ie ? tree_ofSeq(comparer, ie) : tree_empty());
}






function tryFind(k, map$$1) {
    return tree_tryFind(map$$1.comparer, k, map$$1.tree);
}

// TODO: should be xs: Iterable<List<T>>



function initialize(n, f) {
    if (n < 0) {
        throw new Error("List length must be non-negative");
    }
    let xs = new List$1();
    for (let i = 1; i <= n; i++) {
        xs = new List$1(f(n - i), xs);
    }
    return xs;
}
function map$1(f, xs) {
    return reverse(fold$1((acc, x) => new List$1(f(x), acc), new List$1(), xs));
}




function reverse(xs) {
    return fold$1((acc, x) => new List$1(x, acc), new List$1(), xs);
}


/* ToDo: instance unzip() */

/* ToDo: instance unzip3() */

const Result$$1 = function (__exports) {
    const map2$$1 = __exports.map2 = function (fn, a, b) {
        const matchValue = [a, b];

        if (matchValue[0].tag === 1) {
            return new Result$1(1, matchValue[0].data);
        } else if (matchValue[1].tag === 1) {
            return new Result$1(1, matchValue[1].data);
        } else {
            return new Result$1(0, fn(matchValue[0].data, matchValue[1].data));
        }
    };

    const combineList = __exports.combineList = function (list) {
        var folder;
        var fn;
        return (folder = (fn = function (e, l) {
            return new List$1(e, l);
        }, function (a, b) {
            return map2$$1(fn, a, b);
        }), function (state) {
            return foldBack$1(folder, list, state);
        })(new Result$1(0, new List$1()));
    };

    const combineArray = __exports.combineArray = function (array) {
        var fn;
        return fold$1((fn = function (a, e) {
            return a.concat.bind(a)(Array.from(singleton$1(e)));
        }, function (a_1, b) {
            return map2$$1(fn, a_1, b);
        }), new Result$1(0, new Array(0)), array);
    };

    const ofOption = __exports.ofOption = function (error, option) {
        if (option != null) {
            return new Result$1(0, getValue(option));
        } else {
            return new Result$1(1, error);
        }
    };

    const ofChoice = __exports.ofChoice = function (choice) {
        if (choice.tag === 1) {
            return new Result$1(1, choice.data);
        } else {
            return new Result$1(0, choice.data);
        }
    };

    const fromResultResult = __exports.fromResultResult = function (resultResult) {
        const $var1 = resultResult.tag === 1 ? [1, resultResult.data] : resultResult.data.tag === 1 ? [1, resultResult.data.data] : [0, resultResult.data.data];

        switch ($var1[0]) {
            case 0:
                return new Result$1(0, $var1[1]);

            case 1:
                return new Result$1(1, $var1[1]);
        }
    };

    const andThen = __exports.andThen = function (func, result) {
        return bind(func, result);
    };

    const Builder = __exports.Builder = class Builder {
        [FSymbol.reflection]() {
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
            return new Result$1(0, x);
        }

        ReturnFrom(m) {
            return m;
        }

        Zero() {
            return new Result$1(0, null);
        }

    };
    setType("Fable.EdIlyin.Core.Result.Builder", Builder);

    const unpack = __exports.unpack = function (errorFunc, okFunc, _arg1) {
        if (_arg1.tag === 0) {
            return okFunc(_arg1.data);
        } else {
            return errorFunc(_arg1.data);
        }
    };

    const unwrap = __exports.unwrap = function (defaultValue, okFunc, _arg1) {
        if (_arg1.tag === 0) {
            return okFunc(_arg1.data);
        } else {
            return defaultValue;
        }
    };

    return __exports;
}({});
const ResultAutoOpen = function (__exports) {
    const result = __exports.result = new Result$$1.Builder();
    return __exports;
}({});

const _Promise = function (__exports) {
    const result = __exports.result = function (a) {
        return a.then($var1 => new Result$1(0, $var1), $var2 => new Result$1(1, $var2));
    };

    const mapResult = __exports.mapResult = function (fn, a) {
        return a.then(function (result_1) {
            return map(fn, result_1);
        });
    };

    const bindResult = __exports.bindResult = function (fn, a) {
        return a.then(function (a_1) {
            return a_1.tag === 1 ? Promise.resolve(new Result$1(1, a_1.data)) : result(fn(a_1.data));
        });
    };

    const mapResultError = __exports.mapResultError = function (fn, a) {
        return a.then(function (result_1) {
            return mapError(fn, result_1);
        });
    };

    const tap = __exports.tap = function (fn, a) {
        return a.then(function (x) {
            fn(x);
            return x;
        });
    };

    const PromiseBuilder = __exports.PromiseBuilder = class PromiseBuilder {
        [FSymbol.reflection]() {
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

// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex

const fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;




function toHex(value) {
    return value < 0
        ? "ff" + (16777215 - (Math.abs(value) - 1)).toString(16)
        : value.toString(16);
}
function printf(input) {
    return {
        input,
        cont: fsFormat(input),
    };
}


function toText(arg) {
    return arg.cont((x) => x);
}

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
                rep = toString$$1(rep);
                break;
            case "A":
                rep = toString$$1(rep, true);
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
function createPrinter(str, cont) {
    const printer = (...args) => {
        // Make a copy as the function may be used several times
        let strCopy = str;
        for (const arg of args) {
            strCopy = formatOnce(strCopy, arg);
        }
        return fsFormatRegExp.test(strCopy)
            ? createPrinter(strCopy, cont)
            : cont(strCopy.replace(/%%/g, "%"));
    };
    // Mark it as curried so it doesn't
    // get wrapped by CurriedLambda
    printer.curried = true;
    return printer;
}
function fsFormat(str) {
    return (cont) => {
        return fsFormatRegExp.test(str)
            ? createPrinter(str, cont)
            : cont(str);
    };
}







/** Validates UUID as specified in RFC4122 (versions 1-5). Trims braces. */

/* tslint:disable */
// From https://gist.github.com/LeverOne/1308368

/** Parse a UUID into it's component bytes */
// Adapted from https://github.com/zefferus/uuid-parse

/** Convert UUID byte array into a string */



function padLeft(str, len, ch, isRight) {
    ch = ch || " ";
    str = String(str);
    len = len - str.length;
    for (let i = 0; i < len; i++) {
        str = isRight ? str + ch : ch + str;
    }
    return str;
}

function flip(func, x, y) {
  return func(y, x);
}

function tuple(x, y) {
  return [x, y];
}

class _Error {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }

    [FSymbol.reflection]() {
        return {
            type: "Fable.EdIlyin.Core.Decode.Error",
            interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
            cases: [["ExpectingButGot", "string", "string"], ["ErrorMessage", "string"]]
        };
    }

    Equals(other) {
        return this === other || this.tag === other.tag && equals(this.data, other.data);
    }

    CompareTo(other) {
        return compareUnions(this, other) | 0;
    }

}

setType("Fable.EdIlyin.Core.Decode.Error", _Error);
class Decoder {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }

    [FSymbol.reflection]() {
        return {
            type: "Fable.EdIlyin.Core.Decode.Decoder",
            interfaces: ["FSharpUnion"],
            cases: [["Decoder", "string", FableFunction([GenericParam("From"), makeGeneric(Result$1, {
                T: GenericParam("To"),
                TError: _Error
            })])]]
        };
    }

}
setType("Fable.EdIlyin.Core.Decode.Decoder", Decoder);
function run(_arg1, input) {
    return _arg1.data[1](input);
}
function getLabel(_arg1) {
    return _arg1.data[0];
}
function decode(decoder, source) {
    const matchValue = run(decoder, source);

    if (matchValue.tag === 1) {
        if (matchValue.data.tag === 1) {
            const error = matchValue.data.data;
            return new Result$1(1, error);
        } else {
            const got = matchValue.data.data[1];
            const expecting = matchValue.data.data[0];
            return new Result$1(1, toText(printf("Expecting %s, but instead got: %A"))(expecting, got));
        }
    } else {
        return new Result$1(0, matchValue.data);
    }
}
function primitive(expecting, func) {
    return new Decoder(0, [expecting, func]);
}
function fromFunction(func) {
    return primitive("", func);
}
function fail(error) {
    return fromFunction(function (_arg1) {
        return new Result$1(1, new _Error(1, error));
    });
}
function succeed(value) {
    return fromFunction(function (_arg1) {
        return new Result$1(0, value);
    });
}

function expectingButGot(expecting_1, got) {
    return new Result$1(1, new _Error(0, [expecting_1, toText(printf("%A"))(got)]));
}
function andThen(func, decoder) {
    const label = getLabel(decoder);
    return function (func_1) {
        return primitive(label, func_1);
    }(function (input) {
        const matchValue = run(decoder, input);

        if (matchValue.tag === 1) {
            return new Result$1(1, matchValue.data);
        } else {
            return run(func(matchValue.data), input);
        }
    });
}
class Builder {
    [FSymbol.reflection]() {
        return {
            type: "Fable.EdIlyin.Core.Decode.Builder",
            properties: {}
        };
    }

    constructor() {}

    Bind(m, f) {
        return andThen(f, m);
    }

    Return(m) {
        return succeed(m);
    }

}
setType("Fable.EdIlyin.Core.Decode.Builder", Builder);
function andMap(decoder, functionDecoder) {
    return andThen(function (f) {
        return andThen($var1 => succeed(f($var1)), decoder);
    }, functionDecoder);
}
function map$5(func, decoder) {
    return function (functionDecoder) {
        return andMap(decoder, functionDecoder);
    }(succeed(func));
}








function fromResult(result) {
    return Result$$1.unpack(fail, succeed, result);
}

function result(decoder) {
    return fromFunction($var2 => function (arg0) {
        return new Result$1(0, arg0);
    }(function (source) {
        return decode(decoder, source);
    }($var2)));
}


function fromDecodeResult(decodeResult) {
    return fromFunction(function (_arg1) {
        return decodeResult;
    });
}



function orElse(decoder2, decoder1) {
    return andThen(function (_arg1) {
        return _arg1.tag === 0 ? fromResult(new Result$1(0, _arg1.data)) : decoder2;
    }, result(decoder1));
}
function oneOf(decoderList) {
    var func;
    return reduce((func = orElse, function (x, y) {
        return flip(func, x, y);
    }), decoderList);
}

// TODO: This needs improvement, check namespace for non-custom types?

// tslint:disable:max-line-length
// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies

// tslint:disable:ban-types



// TODO: Dates and types with `toJSON` are not adding the $type field

const Fetch_types = function (__exports) {
        const HttpRequestHeaders = __exports.HttpRequestHeaders = class HttpRequestHeaders {
                constructor(tag, data) {
                        this.tag = tag | 0;
                        this.data = data;
                }

                [FSymbol.reflection]() {
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
                        this.tag = tag | 0;
                        this.data = data;
                }

                [FSymbol.reflection]() {
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
    return fold$1(function (o, tupledArg) {
        o[tupledArg[0]] = tupledArg[1];
        return o;
    }, {}, fields);
}
function string(x) {
    return x;
}

const PromiseResult = function (__exports) {
    const andThen = __exports.andThen = function (func, promiseResult) {
        return function (builder_) {
            return builder_.Delay(function () {
                return promiseResult.then(function (_arg1) {
                    return (_arg1.tag === 0 ? func(_arg1.data) : function (builder__1) {
                        return builder__1.Delay(function () {
                            return Promise.resolve(new Result$1(1, _arg1.data));
                        });
                    }(PromiseImpl.promise)).then(function (_arg2) {
                        return Promise.resolve(_arg2);
                    });
                });
            });
        }(PromiseImpl.promise);
    };

    const result = __exports.result = function (value) {
        return function (builder_) {
            return builder_.Delay(function () {
                return Promise.resolve(new Result$1(0, value));
            });
        }(PromiseImpl.promise);
    };

    const Builder = __exports.Builder = class Builder {
        [FSymbol.reflection]() {
            return {
                type: "Fable.EdIlyin.Core.PromiseResult.Builder",
                properties: {}
            };
        }

        constructor() {}

        Bind(m, f) {
            return andThen(f, m);
        }

        Return(m) {
            return result(m);
        }

    };
    setType("Fable.EdIlyin.Core.PromiseResult.Builder", Builder);

    const mapError$$1 = __exports.mapError = function (func, promiseResult) {
        return function (builder_) {
            return builder_.Delay(function () {
                return promiseResult.then(function (_arg1) {
                    return Promise.resolve(_arg1.tag === 1 ? new Result$1(1, func(_arg1.data)) : new Result$1(0, _arg1.data));
                });
            });
        }(PromiseImpl.promise);
    };

    return __exports;
}({});
const PromiseResultAutoOpen = function (__exports) {
    const promiseResult = __exports.promiseResult = new PromiseResult.Builder();
    return __exports;
}({});

function decodeValue(decoder, jsonValue) {
    return decode(decoder, jsonValue);
}
function decodeString(decoder, jsonString) {
    const pojo = JSON.parse(jsonString);
    return decodeValue(decoder, pojo);
}
const value = primitive("a POJO", function (arg0) {
    return new Result$1(0, arg0);
});
function field(name, decoder) {
    const label = toText(printf("%s field '%s'"))(getLabel(decoder), name);
    return primitive(label, function (o) {
        const matchValue = Object.prototype.hasOwnProperty.call(o, name);

        if (matchValue) {
            return function (input) {
                return run(decoder, input);
            }(o[name]);
        } else {
            return expectingButGot(label, o);
        }
    });
}

const bool$1 = primitive("a Bool", function (o) {
    return typeof o === "boolean" ? new Result$1(0, o) : expectingButGot("a Bool", o);
});
const uint16$1 = primitive("an UInt16", function (o) {
    return (value => {
        if (typeof value !== "number") {
            return false;
        }

        

        if (0 <= value && value <= 65535 && (value | 0) === value) {
            return true;
        }

        if (isFinite(value) && !(value % 1)) {
            return true;
        }

        
        return false;
    })(o) ? new Result$1(0, o) : expectingButGot("an UInt16", o);
});
const uint32$1 = primitive("an UInt32", function (o) {
    return (value => {
        if (typeof value !== "number") {
            return false;
        }

        

        if (0 <= value && value <= 4294967295 && (value | 0) === value) {
            return true;
        }

        if (isFinite(value) && !(value % 1)) {
            return true;
        }

        
        return false;
    })(o) ? new Result$1(0, o) : expectingButGot("an UInt32", o);
});
const uint64$1 = primitive("an UInt64", function (o) {
    return (value => {
        if (typeof value !== "number") {
            return false;
        }

        

        if (0 <= value && value <= 18446744073709551615 && (value | 0) === value) {
            return true;
        }

        if (isFinite(value) && !(value % 1)) {
            return true;
        }

        
        return false;
    })(o) ? new Result$1(0, o) : expectingButGot("an UInt64", o);
});
const int16$1 = primitive("an Int16", function (o) {
    return (value => {
        if (typeof value !== "number") {
            return false;
        }

        

        if (-32768 <= value && value <= 32767 && (value | 0) === value) {
            return true;
        }

        if (isFinite(value) && !(value % 1)) {
            return true;
        }

        
        return false;
    })(o) ? new Result$1(0, o) : expectingButGot("an Int16", o);
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
    })(o) ? new Result$1(0, o) : expectingButGot("an Int", o);
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
    })(o) ? new Result$1(0, o) : expectingButGot("an Int64", o);
});

const _float$1 = primitive("a Float", function (o) {
    return typeof o === "number" ? new Result$1(0, o) : expectingButGot("a Float", o);
});

function dict(decoder) {
    const label = toText(printf("maping of string to %s"))(getLabel(decoder));
    return andThen(function (o) {
        return fromResult(map(function (elements) {
            return create$1(elements, new Comparer(comparePrimitives));
        }, Result$$1.combineList(map$1(function (tupledArg) {
            return map(function (y) {
                return tuple(tupledArg[0], y);
            }, decode(decoder, tupledArg[1]));
        }, Object.entries(o)))));
    }, value);
}
const dateTime = map$5(function (i) {
    const start = create(1970, 1, 1, 0, 0, 0, 0, 1);
    return addSeconds(start, i);
}, _float$1);
const string$1 = primitive("a String", function (o) {
    return typeof o === "string" ? new Result$1(0, o) : function (got) {
        return expectingButGot("a String", got);
    }(o);
});



function index(i, decoder) {
    return primitive("an array", function (array_1) {
        return array_1 instanceof Array ? i >= array_1.length ? expectingButGot(toText(printf("a longer array. Need index %i"))(i), array_1) : run(decoder, array_1[i]) : expectingButGot("an array", array_1);
    });
}
function Null$1(a) {
    return primitive("a Null", function (o) {
        return o === null ? new Result$1(0, a) : expectingButGot("a Null", o);
    });
}
function nullable(decoder) {
    return oneOf(ofArray([Null$1(null), map$5(makeSome, decoder)]));
}

es6Promise.polyfill();

function _fetch(url, properties, decoder) {
    return _Promise.result(function (builder_) {
        return builder_.Delay(function () {
            return fetch(url, createObj(properties, 1)).then(function (_arg1) {
                return Result$$1.unpack($var2 => function (arg00) {
                    return Promise.resolve(arg00);
                }(function (arg0) {
                    return new Result$1(1, arg0);
                }($var2)), function (x_1) {
                    return x_1;
                }, decode(decoder, _arg1)).then(function (_arg2) {
                    return Promise.resolve(_arg2);
                });
            });
        });
    }(PromiseImpl.promise)).then($var1 => Result$$1.andThen(function (x) {
        return x;
    }, mapError(function (e) {
        return e.message;
    }, $var1)));
}

function get(url, headers, decoder) {
    const properties = ofArray([new Fetch_types.RequestProperties(0, "GET"), new Fetch_types.RequestProperties(1, createObj(headers, 0))]);
    return _fetch(url, properties, decoder);
}


const text = primitive("a Text", function (response) {
    return new Result$1(0, PromiseResult.mapError(function (e) {
        return e.message;
    }, _Promise.result(response.text())));
});
function json(decoder) {
    return primitive("an JSON", function (response) {
        return new Result$1(0, function (builder_) {
            return builder_.Delay(function () {
                return response.json().then(function (_arg1) {
                    const result$$1 = decodeValue(decoder, _arg1);
                    return Promise.resolve(result$$1);
                });
            });
        }(PromiseImpl.promise));
    });
}
const response = primitive("an HTTP response", $var4 => function (arg0_1) {
    return new Result$1(0, arg0_1);
}(($var3 => function (arg00) {
    return Promise.resolve(arg00);
}(function (arg0) {
    return new Result$1(0, arg0);
}($var3)))($var4)));

function equal(expected, actual) {
    const assert_ = assert;
    assert_.deepStrictEqual(actual, expected);
}
it("fetch: json echo", function () {
    return function (builder_) {
        return builder_.Delay(function () {
            return get("http://echo.jsontest.com/abba/babba", new List$1(), json(value)).then(function (_arg1) {
                const result = equal(new Result$1(0, object(ofArray([["abba", string("babba")]]))), _arg1);
                return Promise.resolve(null);
            });
        });
    }(PromiseImpl.promise);
});
it("fetch: wrong address", function () {
    return function (builder__1) {
        return builder__1.Delay(function () {
            return get("http://echoa.jsontest.com", new List$1(), text).then(function (_arg1_1) {
                const result_1 = equal(new Result$1(1, "request to http://echoa.jsontest.com failed, reason: getaddrinfo ENOTFOUND echoa.jsontest.com echoa.jsontest.com:80"), _arg1_1);
                return Promise.resolve(null);
            });
        });
    }(PromiseImpl.promise);
});
it("fetch: json echo with decoder", function () {
    return function (builder__2) {
        return builder__2.Delay(function () {
            return get("http://echo.jsontest.com/abba/babba", new List$1(), json(field("abba", string$1))).then(function (_arg1_2) {
                const result_2 = equal(new Result$1(0, "babba"), _arg1_2);
                return Promise.resolve(null);
            });
        });
    }(PromiseImpl.promise);
});
it("fetch: json echo with decoder: error", function () {
    return function (builder__3) {
        return builder__3.Delay(function () {
            return get("http://echo.jsontest.com/abba/babba", new List$1(), json(field("abbax", string$1))).then(function (_arg1_3) {
                const result_3 = equal(new Result$1(1, "Expecting a String field 'abbax', but instead got: \"{\\\"abba\\\":\\\"babba\\\"}\""), _arg1_3);
                return Promise.resolve(null);
            });
        });
    }(PromiseImpl.promise);
});

it("result: computation expression: return", function () {
    const assert_ = assert;
    assert_.deepStrictEqual(function (builder_) {
        return builder_.Bind(new Result$1(0, 42), builder_.Return.bind(builder_));
    }(ResultAutoOpen.result), new Result$1(0, 42));
});
it("result: computation expression: return from", function () {
    const assert__1 = assert;
    assert__1.deepStrictEqual(function (builder__1) {
        const r = new Result$1(0, 42);
        return builder__1.ReturnFrom(r);
    }(ResultAutoOpen.result), new Result$1(0, 42));
});
it("result: computation expression: zero", function () {
    const assert__2 = assert;
    assert__2.deepStrictEqual(function (builder__2) {
        toText(printf("%i"))(42);
        return builder__2.Zero();
    }(ResultAutoOpen.result), new Result$1(0, null));
});

class queue {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }

    [FSymbol.reflection]() {
        return {
            type: "Fable.EdIlyin.Core.Queue.queue",
            interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
            cases: [["Queue", makeGeneric(List$1, {
                T: GenericParam("a")
            }), makeGeneric(List$1, {
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
    return new queue(0, [new List$1(), new List$1()]);
}

function push(_arg1, item) {
    return new queue(0, [_arg1.data[0], new List$1(item, _arg1.data[1])]);
}
function ofList$1(list) {
    return new queue(0, [list, new List$1()]);
}

function pull(_arg1) {
    pull: while (true) {
        if (_arg1.data[0].tail != null) {
            return [_arg1.data[0].head, new queue(0, [_arg1.data[0].tail, _arg1.data[1]])];
        } else if (_arg1.data[1].tail == null) {
            return null;
        } else {
            _arg1 = ofList$1(reverse(_arg1.data[1]));
            continue pull;
        }
    }
}
function length(_arg1) {
    return _arg1.data[0].length + _arg1.data[1].length | 0;
}

const parseRadix = /^\s*([\+\-])?(0[xob])?([0-9a-fA-F]+)\s*$/;
const invalidRadix2 = /[^01]/;
const invalidRadix8 = /[^0-7]/;
const invalidRadix10 = /[^0-9]/;
function isValid(s, radix) {
    const res = parseRadix.exec(s);
    if (res != null) {
        if (radix == null) {
            switch (res[2]) {
                case "0b":
                    radix = 2;
                    break;
                case "0o":
                    radix = 8;
                    break;
                case "0x":
                    radix = 16;
                    break;
                default:
                    radix = 10;
                    break;
            }
        }
        switch (radix) {
            case 2:
                return invalidRadix2.test(res[3]) ? null : [res, 2];
            case 8:
                return invalidRadix8.test(res[3]) ? null : [res, 8];
            case 10:
                return invalidRadix10.test(res[3]) ? null : [res, 10];
            case 16:
                return [res, 16];
            default:
                throw new Error("Invalid Base.");
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
    toJSON() {
        return (!this.unsigned && !this.lessThan(0) ? "+" : "") + this.toString();
    }
    static ofJSON(str) {
        return fromString(str, !/^[+-]/.test(str));
    }
    [FSymbol.reflection]() {
        return {
            type: this.unsigned ? "System.UInt64" : "System.Int64",
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
function unixEpochMillisecondsToTicks(ms, offset) {
    return fromNumber(ms).add(62135596800000).add(offset).mul(10000);
}

// TimeSpan in runtime just becomes a number representing milliseconds
function create$5(d = 0, h = 0, m = 0, s = 0, ms = 0) {
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

class OperationCanceledError extends Error {
    constructor() {
        super("The operation was canceled");
        Object.setPrototypeOf(this, OperationCanceledError.prototype);
    }
}
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
            ctx.onCancel(new OperationCanceledError());
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
const singleton$3 = new AsyncBuilder();

function op_GreaterGreaterEquals(asyn, func) {
  return singleton$3.Bind(asyn, func);
}
function map$7(func, asyn) {
  return op_GreaterGreaterEquals(asyn, $var1 => singleton$3.Return.bind(singleton$3)(func($var1)));
}
function op_BarGreaterGreater(asyn, func) {
  return map$7(func, asyn);
}

function emptyContinuation(x) {
    // NOP
}





function awaitPromise(p) {
    return fromContinuations((conts) => p.then(conts[0]).catch((err) => (err instanceof OperationCanceledError
        ? conts[2] : conts[1])(err)));
}

const defaultCancellationToken = { isCancelled: false };

function fromContinuations(f) {
    return protectedCont((ctx) => f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
}

function parallel(computations) {
    return awaitPromise(Promise.all(map$3((w) => startAsPromise(w), computations)));
}
function sleep(millisecondsDueTime) {
    return protectedCont((ctx) => {
        setTimeout(() => ctx.cancelToken.isCancelled
            ? ctx.onCancel(new OperationCanceledError())
            : ctx.onSuccess(void 0), millisecondsDueTime);
    });
}
function start$1(computation, cancellationToken) {
    return startWithContinuations(computation, cancellationToken);
}
function startImmediate(computation, cancellationToken) {
    return start$1(computation, cancellationToken);
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

    [FSymbol.reflection]() {
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
        this.tag = tag | 0;
        this.data = data;
    }

    [FSymbol.reflection]() {
        return {
            type: "Fable.EdIlyin.Core.Throttle.Msg",
            interfaces: ["FSharpUnion"],
            cases: [["Die"], ["Fetch", FableFunction([Unit, GenericParam("a")]), Any]]
        };
    }

}
setType("Fable.EdIlyin.Core.Throttle.Msg", Msg);
function nowMilliseconds() {
    var copyOfStruct;
    const milliseconds$$1 = create$5((copyOfStruct = now(), unixEpochMillisecondsToTicks(copyOfStruct.getTime(), offset(copyOfStruct))));
    return milliseconds$$1;
}

function execute(func, channel, model) {
    channel.reply.bind(channel)(func());
    const queue$$1 = push(model.queue, nowMilliseconds());
    return new Model(model.quantity, model.millisecond, queue$$1);
}

function _fetch$2(model, func, channel) {
    return map$7(function (model_1) {
        return execute(func, channel, model_1);
    }, (() => {
        const matchValue = length(model.queue) | 0;

        if (matchValue < model.quantity) {
            return singleton$3.Return(model);
        } else {
            const matchValue_1 = pull(model.queue);

            if (matchValue_1 != null) {
                const was = getValue(matchValue_1)[0];
                const tail = getValue(matchValue_1)[1];
                return op_BarGreaterGreater(sleep(model.millisecond - ~~(nowMilliseconds() - was)), function () {
                    return new Model(model.quantity, model.millisecond, tail);
                });
            } else {
                return singleton$3.Return(model);
            }
        }
    })());
}

function body(model, agent) {
    const loop = function (state) {
        return op_GreaterGreaterEquals(agent.receive(), function (_arg1) {
            return _arg1.tag === 1 ? op_GreaterGreaterEquals(_fetch$2(state, _arg1.data[0], _arg1.data[1]), loop) : singleton$3.Zero();
        });
    };

    return loop(model);
}

function start$$1(quantity, millisecond$$1) {
    var model;
    return start$2((model = new Model(quantity, millisecond$$1, empty$1()), function (agent) {
        return body(model, agent);
    }));
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
    const throttler = start$$1(1, 1000);

    const func = function () {
        return 42;
    };

    return startAsPromise(function (builder_) {
        return builder_.Delay(function () {
            return builder_.Bind(add$4(throttler, func), function (_arg1) {
                return builder_.Return(equal$1(42, _arg1));
            });
        });
    }(singleton$3));
});
it("throttle: async function", function () {
    const throttler_1 = start$$1(2, 1000);

    const func_1 = function () {
        return function (builder__1) {
            return builder__1.Delay(function () {
                return builder__1.Return(42);
            });
        }(singleton$3);
    };

    return startAsPromise(function (builder__2) {
        return builder__2.Delay(function () {
            return builder__2.Bind(add$4(throttler_1, func_1), function (_arg1_1) {
                return builder__2.Bind(_arg1_1, function (_arg2) {
                    return builder__2.Return(equal$1(42, _arg2));
                });
            });
        });
    }(singleton$3));
});
function multipleFunTest(func_2, unitVar1) {
    const throttler_2 = start$$1(3, 100);
    return startAsPromise(function (builder__3) {
        return builder__3.Delay(function () {
            return builder__3.Bind(parallel(initialize(22, function (_arg1_2) {
                return func_2(function (func_3) {
                    return add$4(throttler_2, func_3);
                });
            })), function (_arg1_3) {
                var loop;
                const results = map2(function (tupledArg, x) {
                    return tupledArg[0] <= x ? x <= tupledArg[1] : false;
                }, (loop = function () {
                    return delay(function () {
                        return append$1(singleton$1([0, 0]), delay(function () {
                            return append$1(singleton$1([0, 10]), delay(function () {
                                return append$1(singleton$1([90, 110]), delay(loop));
                            }));
                        }));
                    });
                }, loop()), map$4(function (tupledArg_1) {
                    return ~~(tupledArg_1[1] - tupledArg_1[0]);
                }, Array.from(pairwise(_arg1_3)), Int32Array));
                equal$1(initialize$1(22 - 1, function (_arg2_1) {
                    return true;
                }), results);
                return builder__3.Zero();
            });
        });
    }(singleton$3));
}
it("throttle: multiple simple functions", (() => {
    const func_2 = function (throttle) {
        return throttle(nowMilliseconds);
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
            }(singleton$3);
        };

        return function (builder__4) {
            return builder__4.Delay(function () {
                return builder__4.Bind(throttle_1(func_3), builder__4.ReturnFrom.bind(builder__4));
            });
        }(singleton$3);
    };

    return function () {
        return multipleFunTest(func_4, null);
    };
})());
class DifferentResult {
    constructor(tag, data) {
        this.tag = tag | 0;
        this.data = data;
    }

    [FSymbol.reflection]() {
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
    const throttler_2 = start$$1(4, 100);

    const throttle_2 = function (func_5) {
        return add$4(throttler_2, func_5);
    };

    const patternInput = [42, "thirty two"];

    const func1 = function () {
        return patternInput[0];
    };

    const func2 = function () {
        return patternInput[1];
    };

    return startAsPromise(function (builder__5) {
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
    }(singleton$3));
});

// TODO does this perfectly match the .NET behavior ?
function tryParse$3(s, radix, initial) {
    if (s != null && /\S/.test(s)) {
        if (radix === 10) {
            const v = +s;
            if (!Number.isNaN(v)) {
                return [true, v];
            }
        }
    }
    return [false, initial != null ? initial : 0];
}
function parse$3(s, radix = 10) {
    const a = tryParse$3(s, radix, 0);
    if (a[0]) {
        return a[1];
    }
    else {
        // TODO FormatException ?
        throw new Error("Input string was not in a correct format.");
    }
}

function equal$2(expected, actual) {
    const assert_ = assert;
    assert_.deepStrictEqual(actual, expected);
}
it("json decode: null", function () {
    equal$2(new Result$1(0, true), decodeString(Null$1(true), "null"));
});
it("json decode: nullable int: 42", function () {
    equal$2(new Result$1(0, 42), decodeString(nullable(_int$1), "42"));
});
it("json decode: nullable int: null", function () {
    equal$2(new Result$1(0, null), decodeString(nullable(_int$1), "null"));
});
it("json decode: index: 42", function () {
    equal$2(new Result$1(0, 42), decodeString(index(1, _int$1), "[12,42,43]"));
});
it("json decode: index: nullable int: 42", function () {
    equal$2(new Result$1(0, 42), decodeString(index(1, nullable(_int$1)), "[12,42,43]"));
});
it("json decode: index: nullable int: null", function () {
    equal$2(new Result$1(0, null), decodeString(index(1, nullable(_int$1)), "[12,null,43]"));
});
it("json decode: index last element", function () {
    equal$2(new Result$1(0, 43), decodeString(index(2, nullable(_int$1)), "[12,null,43]"));
});
it("json decode: index out of length", function () {
    equal$2(new Result$1(1, "Expecting a longer array. Need index 3, but instead got: \"[12,null,43]\""), decodeString(index(3, nullable(_int$1)), "[12,null,43]"));
});
const floatFromString = andThen(function (s) {
    return fromDecodeResult((() => {
        try {
            return new Result$1(0, parse$3(s));
        } catch (e) {
            return expectingButGot("a Float in String", s);
        }
    })());
}, string$1);
it("json decode: wrong field name", function () {
    equal$2(new Result$1(1, "Expecting a String field 'lowerAsk', but instead got: \"{\\\"last\\\":\\\"0.00007602\\\",\\\"lowestAsk\\\":\\\"0.00007602\\\"}\""), decodeString(field("lowerAsk", floatFromString), "{\"last\":\"0.00007602\",\"lowestAsk\":\"0.00007602\"}"));
});
it("json decode: dict: wrong field name", function () {
    equal$2(new Result$1(1, "Expecting a String field 'lowerAsk', but instead got: \"{\\\"a\\\":1,\\\"b\\\":2}\""), decodeString(dict(field("lowerAsk", floatFromString)), "{\"one\":{\"a\":1,\"b\":2},\"two\":{\"a\":2,\"b\":3}}"));
});
