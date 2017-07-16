'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var assert = require('assert');

const types = new Map();
function setType(fullName, cons) {
    types.set(fullName, cons);
}

var FSymbol = {
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
        const info = obj[FSymbol.reflection]();
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

// This module is split from List.ts to prevent cyclic dependencies

class List$1 {
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
    [FSymbol.reflection]() {
        return {
            type: "Microsoft.FSharp.Collections.FSharpList",
            interfaces: ["System.IEquatable", "System.IComparable"],
        };
    }
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







function initialize$1(n, f) {
    return delay(() => unfold((i) => i < n ? [f(i), i + 1] : null, 0));
}










// A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442

function map$2(f, xs) {
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

function singleton$2(y) {
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

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies

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




function reverse(xs) {
    return fold$1((acc, x) => new List$1(x, acc), new List$1(), xs);
}


/* ToDo: instance unzip() */

/* ToDo: instance unzip3() */

class queue {
  constructor(tag, data) {
    this.tag = tag;
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
function empty() {
  return new queue(0, [new List$1(), new List$1()]);
}

function push(_arg1, item) {
  return new queue(0, [_arg1.data[0], new List$1(item, _arg1.data[1])]);
}
function ofList(list) {
  return new queue(0, [list, new List$1()]);
}

function pull(_arg1) {
  pull: while (true) {
    if (_arg1.data[0].tail != null) {
      return [_arg1.data[0].head, new queue(0, [_arg1.data[0].tail, _arg1.data[1]])];
    } else if (_arg1.data[1].tail == null) {
      return null;
    } else {
      _arg1 = ofList(reverse(_arg1.data[1]));
      continue pull;
    }
  }
}
function length(_arg1) {
  return _arg1.data[0].length + _arg1.data[1].length | 0;
}

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
    [FSymbol.reflection]() {
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

function create$1(d = 0, h = 0, m = 0, s = 0, ms = 0) {
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
function parse$2(v, kind) {
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


function now() {
    return parse$2();
}

















function ticks$1(d) {
    return fromNumber(d.getTime())
        .add(62135596800000) // UnixEpochMilliseconds
        .sub(d.kind === 2 /* Local */ ? d.getTimezoneOffset() * 60 * 1000 : 0)
        .mul(10000);
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
const singleton$3 = new AsyncBuilder();

function emptyContinuation(x) {
    // NOP
}
function awaitPromise(p) {
    return fromContinuations((conts) => p.then(conts[0]).catch((err) => (err === "cancelled" ? conts[2] : conts[1])(err)));
}

const defaultCancellationToken = { isCancelled: false };

function fromContinuations(f) {
    return protectedCont((ctx) => f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
}

function parallel(computations) {
    return awaitPromise(Promise.all(map$2((w) => startAsPromise(w), computations)));
}
function sleep(millisecondsDueTime) {
    return protectedCont((ctx) => {
        setTimeout(() => ctx.cancelToken.isCancelled ?
            ctx.onCancel("cancelled") : ctx.onSuccess(void 0), millisecondsDueTime);
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
    this.tag = tag;
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
  const milliseconds$$1 = create$1((() => {
    let copyOfStruct = now();
    return ticks$1(copyOfStruct);
  })());
  return ~~milliseconds$$1 | 0;
}

function execute(func, channel, model) {
  (function (arg00) {
    channel.reply(arg00);
  })(func());

  const queue$$1 = push(model.queue, nowMilliseconds());
  return new Model(model.quantity, model.millisecond, queue$$1);
}

function _fetch(model, func, channel) {
  return singleton$3.Bind((() => {
    const matchValue = length(model.queue) | 0;

    if (matchValue < model.quantity) {
      return singleton$3.Return(model);
    } else {
      const matchValue_1 = pull(model.queue);

      if (matchValue_1 != null) {
        const was = matchValue_1[0] | 0;
        const tail = matchValue_1[1];
        return singleton$3.Bind(sleep(model.millisecond - (nowMilliseconds() - was)), $var1 => function (arg00) {
          return singleton$3.Return(arg00);
        }(function () {
          return new Model(model.quantity, model.millisecond, tail);
        }($var1)));
      } else {
        return singleton$3.Return(model);
      }
    }
  })(), $var2 => function (arg00_1) {
    return singleton$3.Return(arg00_1);
  }(function (model_1) {
    return execute(func, channel, model_1);
  }($var2)));
}

function body(model, agent) {
  const loop = function (state) {
    return singleton$3.Bind(agent.receive(), function (_arg1) {
      return _arg1.tag === 1 ? singleton$3.Bind(_fetch(state, _arg1.data[0], _arg1.data[1]), loop) : singleton$3.Zero();
    });
  };

  return loop(model);
}

function start(quantity, millisecond$$1) {
  return function (arg00) {
    return start$2(arg00);
  }((() => {
    const model = new Model(quantity, millisecond$$1, empty());
    return function (agent) {
      return body(model, agent);
    };
  })());
}
function add(throttler, func) {
  return throttler.postAndAsyncReply(function (channel) {
    return new Msg(1, [func, channel]);
  });
}

it("throttle: simple function", function () {
  const throttler = start(1, 1000);

  const func = function () {
    return 42;
  };

  return function (arg00) {
    return startAsPromise(arg00);
  }(function (builder_) {
    return builder_.Delay(function () {
      return builder_.Bind(add(throttler, func), function (_arg1) {
        return builder_.Return((() => {
          const assert_ = assert;
          assert_.deepStrictEqual(_arg1, 42);
        })());
      });
    });
  }(singleton$3));
});
it("throttle: async function", function () {
  const throttler_1 = start(2, 1000);

  const func_1 = function () {
    return function (builder__1) {
      return builder__1.Delay(function () {
        return builder__1.Return(42);
      });
    }(singleton$3);
  };

  return function (arg00_1) {
    return startAsPromise(arg00_1);
  }(function (builder__2) {
    return builder__2.Delay(function () {
      return builder__2.Bind(add(throttler_1, func_1), function (_arg1_1) {
        return builder__2.Bind(_arg1_1, function (_arg2) {
          return builder__2.Return((() => {
            const assert__1 = assert;
            assert__1.deepStrictEqual(_arg2, 42);
          })());
        });
      });
    });
  }(singleton$3));
});
function multipleFunTest(func_2, unitVar1) {
  const throttler_2 = start(3, 100);
  return function (arg00_2) {
    return startAsPromise(arg00_2);
  }(function (builder__3) {
    return builder__3.Delay(function () {
      return builder__3.Bind(parallel(initialize(22, function (_arg1_2) {
        return func_2(throttler_2);
      })), function (_arg1_3) {
        const results = map2(function (tupledArg, x) {
          return tupledArg[0] <= x ? x <= tupledArg[1] : false;
        }, (() => {
          const loop = function () {
            return delay(function () {
              return append$1(singleton$2([0, 0]), delay(function () {
                return append$1(singleton$2([0, 10]), delay(function () {
                  return append$1(singleton$2([90, 110]), delay(function () {
                    return loop();
                  }));
                }));
              }));
            });
          };

          return loop();
        })(), Int32Array.from(map$2(function (tupledArg_1) {
          return tupledArg_1[1] - tupledArg_1[0];
        }, Array.from(pairwise(_arg1_3)))));
        const assert__2 = assert;
        assert__2.deepStrictEqual(results, initialize$1(22 - 1, function (_arg2_1) {
          return true;
        }));
        return builder__3.Zero();
      });
    });
  }(singleton$3));
}
it("throttle: multiple simple functions", (() => {
  const func_2 = function (throttler_2) {
    return add(throttler_2, function () {
      return nowMilliseconds();
    });
  };

  return function () {
    return multipleFunTest(func_2, null);
  };
})());
it("throttle: multiple async functions", (() => {
  const func_4 = function (throttler_3) {
    const func_3 = function () {
      return function (builder__3) {
        return builder__3.Delay(function () {
          return builder__3.Return(nowMilliseconds());
        });
      }(singleton$3);
    };

    return function (builder__4) {
      return builder__4.Delay(function () {
        return builder__4.Bind(add(throttler_3, func_3), function (_arg1_2) {
          return builder__4.ReturnFrom(_arg1_2);
        });
      });
    }(singleton$3);
  };

  return function () {
    return multipleFunTest(func_4, null);
  };
})());
