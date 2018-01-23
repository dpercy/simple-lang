

export function $boolean$63$(v) { return typeof v === "boolean"; }
export function $int$63$(v) { return Number.isInteger(v); }
export function $$43$(x, y) {
    if (!($int$63$(x) && $int$63$(y)))
        throw "+ expects integers";
    return x + y;
}

export function $_(x, y) {
    if (!($int$63$(x) && $int$63$(y)))
        throw "- expects integers";
    return x - y;
}

export function $$42$(x, y) {
    if (!($int$63$(x) && $int$63$(y)))
        throw "* expects integers";
    return x * y;
}

export function $$47$(x, y) {
    if (!($int$63$(x) && $int$63$(y)))
        throw "/ expects integers";

    // Racket quotient rounds toward zero (truncate),
    // so match that behavior here.
    const result = Math.trunc(x / y);

    // And watch out for division by zero!
    // JS will give you Infinity, but that's not an int.
    if (!$int$63$(result))
        throw "/ result was not an integer";
    return result;
}

export function $$60$(x, y) {
    if (!($int$63$(x) && $int$63$(y)))
        throw "< expects integers";
    return x < y;
}

export function $$61$(x, y) {
    if (!($int$63$(x) && $int$63$(y)))
        throw "= expects integers";
    return x === y;
}



export function $string$63$(v) { return typeof v === 'string'; }

export function $string$61$$63$(x, y) {
    if (!($string$63$(x) && $string$63$(y)))
        throw "string=? expects strings";
    return x === y;
}

export function $string_append(x, y) {
    if (!($string$63$(x) && $string$63$(y)))
        throw "string-append expects strings";
    return x + y;
}

export function $string_length(x) {
    if (!($string$63$(x)))
        throw "string-length expect a string";
    return x.length;
}

export function $substring(x, start, end) {
    if (!($string$63$(x)))
        throw "substring expect a string";
    if (!($int$63$(x) && $int$63$(y)))
        throw "start and end must be integers";
    if (!(0 <= start && start < x.length))
        throw "start is out of range";
    if (!(0 <= end && end < x.length))
        throw "end is out of range";
    if (!(start <= end))
        throw "start/end are backwards";
    return x.slice(start, end);
}

export function $ord(i) {
    // TODO make JS strings work by code points instead?
    if (!($int$63$(i)))
        throw "ord expects an integer";
    return String.fromCharCode(i);
}
    
export function $chr(s) {
    if (!($string$63$(x)))
        throw "chr expects a string";
    if (x.length !== 1)
        throw "chr expects a single character";
    return x.charCodeAt(0);
}

export function $equal$63$(x, y) {
    // equal? works on bools, ints, strings, and structs.
    if (x === y) return true;
    if (typeof x === 'object' && typeof y === 'object') {
        if (x.constructor !== y.constructor) return false;
        
        // Structs are represented as constructor functions,
        // so constructor.length is the constructor arity,
        // which is also the number of fields.
        for (const i=0; i<x.constructor.length; ++i) {
            // If any one field is unequal, x and y are unequal.
            if (!$equal$63$(x[i], y[i])) return false;
        }

        // x and y are only equal if no field is unequal.
        return true;
    } else {
        return false;
    }
}
