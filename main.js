const fs = require('fs');
const peg = require('pegjs');
const grammar = fs.readFileSync('grammar.pegjs').toString();
const parser = peg.generate(grammar);

function run(program) {
    // takes a program ast (list of statements)
    // returns a new program ast where:
    //  - top-level expressions have been reduced to a value
    //  - definitions with no arguments have been reduced to a value
    for (const s of program) {
        if (s.type === "Def") {
            throw new Error("TODO definitions...");
        }
    }
    return program.map(runExpr);
}

/*
What do values look like?
- number
- JS function (like a code pointer)
- user-defined function
- closure
*/
class Closure {
    // TODO this class could be specialized: closure1, closure2, etc
    constructor(func, args) {
        this.func = func;
        this.args = args;
    }
    show() {
        return "(" + [this.func].concat(this.args).map(show).join(" ") + ")";
    }
}
function show(v) {
    if (v.show) {
        return v.show();
    } else if (v.name) {
        return v.name;
    } else {
        return v.toString();
    }
}

function runExpr(expr) {
    switch (expr.type) {
    case "Num": return +expr.literal;
    case "Id":
        switch (expr.name) {
        case "add": return add;
        default: throw new Error("unbound Id: " + expr.name);
        }
    case "App": {
        // TODO multi-arg call optimization
        const { func, arg } = expr;
        const f = runExpr(func);
        const x = runExpr(arg);
        return apply1(f, x);
    }
    default: throw new Error("bad expr type: " + expr.type);
    }
}

function add(x, y) {
    if (typeof x !== 'number' || typeof y !== 'number')
        throw new Error("TODO deal with errors: add non-number");
    return x + y;
}

function apply1(f, x) {
    if (typeof f === 'function') {
        switch (f.length) {
        case 0: throw new Error("zero arity function??");
        case 1: return f(x);
        default: return new Closure(f, [x]);
        }
    } else if (f instanceof Closure) {
        const { func, args } = f;
        const newArgs = args.concat([x]);
        if (newArgs.length < func.length) {
            return new Closure(func, newArgs);
        } else {
            // TODO what if func is not a prim? then push explicit stack frame.
            return func.apply(null, newArgs);
        }
    } else {
        throw new Error("TODO deal with errors: call non-function");
    }
}

if (require.main === module) {
    const [_node, _main, ...args] = process.argv;

    console.log(args);
    
    for (const filename of args) {
        const text = fs.readFileSync(filename).toString();
        const program = parser.parse(text);
        const result = run(program);
        for (const val of result) {
            console.log(show(val));
        }
    }
}
