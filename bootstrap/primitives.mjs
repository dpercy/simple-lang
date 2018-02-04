import bigInt from "./BigInteger.mjs";

export { bigInt };

export function showError(e) {
    return "(error " + show(e) + ")";
}

export function show(v) {
    switch (typeof v) {
    case 'boolean':
        return v ? "#true" : "#false";
    case 'string':
        return showString(v);
    case 'function':
        return v.schemeName || showRaw(v);
    case 'object': {
        if ($int$63$(v)) {
            return v.toString();
        }
        if (!v.constructor.schemeName) {
            // If the object's class has no schemeName, it must be a raw JS object.
            return showRaw(v);
        }

        // TODO simplify by giving each struct a toString
        let s = '(' + v.constructor.schemeName;
        for (let i=0; i<v.constructor.length; ++i) {
            s += ' ' + show(v[i]);
        }
        s += ')';
        return s;
    }
        
    default: return showRaw(v);
    }
}

function showString(s) {
    // TODO seems like this function should be written in sl...
    // - it's a new case!
    //   a function written in sl,
    //   that the compiler emits calls for.
    //   it's built-in -- but also derived.
    var out = '"';
    for (const c of s) {
        // Keep me in sync with char-escape from compiler.sl ...
        switch (c) {
        case '\0': out += "\\0"; break;
        case '\r': out += "\\r"; break;
        case '\n': out += "\\n"; break;
        case '\t': out += "\\t"; break;
        case '\"': out += "\\\""; break;
        case '\\': out += "\\\\"; break;
        default: out += c; break;
        }
    }
    out += '"';
    return out;
}

function showRaw(v) {
    return "(### RAW JS VALUE ### " + v + " ###)";
}

export function toplevel(name, computeValue) {
    try {
    	const val = computeValue();
        toplevelPrinter(name, val);
	return val;
    } catch(e) {
        if (name) {
            // TODO print errors in failed definitions?
            // it's not really a definition if it fails to define...
        } else {
            console.log(showError(e));
        }
	return undefined;
    }
}

var toplevelPrinter = function(name, val) {
    // By default we only print expressions, not definitions.
    if (!name) {
        console.log(show(val));
    }
};

export function configureRuntime({ toplevelPrinter: tp }) {
    toplevelPrinter = tp;
}

export function $boolean$63$(v) {
    if (arguments.length !== 1)
        throw "boolean?: expected 1 argument but got " + arguments.length;
    return typeof v === "boolean";
}
$boolean$63$.schemeName = "boolean?";

export function $int$63$(v) {
    if (arguments.length !== 1)
        throw "int?: expected 1 argument but got " + arguments.length;
    return bigInt.isInstance(v);
}
$int$63$.schemeName = "int?";

export function $$43$(x, y) {
    if (arguments.length !== 2)
        throw "+: expected 2 arguments but got " + arguments.length;
    if (!($int$63$(x) && $int$63$(y)))
        throw "+: expected an integer";
    return x.plus(y);
}
$$43$.schemeName = "+";

export function $_(x, y) {
    if (arguments.length !== 2)
        throw "-: expected 2 arguments but got " + arguments.length;
    if (!($int$63$(x) && $int$63$(y)))
        throw "-: expected an integer";
    return x.minus(y);
}
$_.schemeName = "-";

export function $$42$(x, y) {
    if (arguments.length !== 2)
        throw "*: expected 2 arguments but got " + arguments.length;
    if (!($int$63$(x) && $int$63$(y)))
        throw "*: expected an integer";
    return x.times(y);
}
$$42$.schemeName = "*";

export function $$47$(x, y) {
    if (arguments.length !== 2)
        throw "/: expected 2 arguments but got " + arguments.length;
    if (!($int$63$(x) && $int$63$(y)))
        throw "/: expected an integer";

    if (y.equals(0)) {
        throw "/: division by zero";
    }

    return x.divide(y);
}
$$47$.schemeName = "/";

export function $$60$(x, y) {
    if (arguments.length !== 2)
        throw "<: expected 2 arguments but got " + arguments.length;
    if (!($int$63$(x) && $int$63$(y)))
        throw "<: expected an integer";
    return x.lesser(y);
}
$$60$.schemeName = "<";

export function $$61$(x, y) {
    if (arguments.length !== 2)
        throw "=: expected 2 arguments but got " + arguments.length;
    if (!($int$63$(x) && $int$63$(y)))
        throw "=: expected an integer";
    return x.equals(y);
}
$$61$.schemeName = "=";


export function $string$63$(v) {
    if (arguments.length !== 1)
        throw "string?: expected 1 argument but got " + arguments.length;
    return typeof v === 'string';
}
$string$63$.schemeName = "string?";

export function $string$61$$63$(x, y) {
    if (arguments.length !== 2)
        throw "string=?: expected 2 arguments but got " + arguments.length;
    if (!($string$63$(x) && $string$63$(y)))
        throw "string=?: expected a string";
    return x === y;
}
$string$61$$63$.schemeName = "string=?";

export function $string_append(x, y) {
    if (arguments.length !== 2)
        throw "string-append: expected 2 arguments but got " + arguments.length;
    if (!($string$63$(x) && $string$63$(y)))
        throw "string-append: expected a string";
    return x + y;
}
$string_append.schemeName = "string-append";

export function $string_length(x) {
    if (arguments.length !== 1)
        throw "string-length: expected 1 argument but got " + arguments.length;
    if (!($string$63$(x)))
        throw "string-length expect a string";
    return bigInt(x.length);
}
$string_length.schemeName = "string-length";

export function $substring(x, start, end) {
    if (arguments.length !== 3)
        throw "substring: expected 3 arguments but got " + arguments.length;
    if (!($string$63$(x))) {
        throw "substring expect a string";
    }
    if (!($int$63$(start) && $int$63$(end))) {
        throw "start and end must be integers";
    }

    // start and end are *slice boundaries*,
    // not indices,
    // so they can be equal to x.length.
    if (!(bigInt.zero.leq(start) && start.leq(x.length))) {
        throw "start is out of range";
    }
    if (!(bigInt.zero.leq(end) && end.leq(x.length))) {
        throw "end is out of range";
    }
    if (!(start.leq(end))) {
        throw "start/end are backwards";
    }
    return x.slice(start, end);
}
$substring.schemeName = "substring";

export function $ord(s) {
    if (arguments.length !== 1)
        throw "ord: expected 1 argument but got " + arguments.length;
    if (!($string$63$(s)))
        throw "ord: expected a string";
    if (s.length !== 1)
        throw "ord: expected a character";
    return bigInt(s.charCodeAt(0));
}
$ord.schemeName = "ord";

export function $chr(i) {
    if (arguments.length !== 1)
        throw "chr: expected 1 argument but got " + arguments.length;
    // TODO make JS strings work by code points instead?
    if (!($int$63$(i))) {
        throw "chr: expected an integer";
    }
    if (!(bigInt.zero.leq(i) && i.leq(127))) {
        throw "chr: expected an ASCII integer";
    }
    return String.fromCharCode(i);
}
$chr.schemeName = "chr";
    
export function $equal$63$(x, y) {
    if (arguments.length !== 2)
        throw "equal?: expected 2 arguments but got " + arguments.length;
    // equal? works on bools, ints, strings, and structs.
    if (x === y) return true;
    if ($int$63$(x)) {
        return $int$63$(y) && x.equals(y);
    }
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
$equal$63$.schemeName = "equal?";

export function isA(val, numParams, ctor) {
    if (!(val instanceof ctor)) return false;
    if (numParams !== ctor.length)
        throw (ctor.schemeName + ": has " + ctor.length + " fields, but pattern has " + numParams);
    return true;
}
