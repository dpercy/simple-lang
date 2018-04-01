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
    const emptyEnv = {};
    const emptyK = (v => v);
    return program.map(expr => runExpr(expr, emptyEnv, emptyK));
}

/*
What do values look like?
- number
- JS function (primitive code pointer)
- Lambda (symbolic, user-defined code pointer)
- Closure
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
class Lambda {
    constructor(name, params, body) {
        this.name = name;
        this.params = params;
        this.body = body;
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

function runExpr(expr, env, k) {
    switch (expr.type) {
    case "Num": return k(+expr.literal);
    case "Id":
        switch (expr.name) {
        case "add": return k(add);
        case "double": return k(exampleLambdaDouble);
        default: {
            if (Object.hasOwnProperty.call(env, expr.name)) {
                return k(env[expr.name]);
            } else {
                throw new Error("unbound Id: " + expr.name);
            }
        }
        }
    case "App": {
        // TODO multi-arg call optimization
        const { func, arg } = expr;
        return runExpr(func, env, (f) => {
            return runExpr(arg, env, (x) => {
                return apply1(f, x, k);
            });
        });
    }
    default: throw new Error("bad expr type: " + expr.type);
    }
}

function add(x, y) {
    if (typeof x !== 'number' || typeof y !== 'number')
        throw new Error("TODO deal with errors: add non-number");
    return x + y;
}

exampleLambdaDouble = new Lambda(
    "double",
    ["x"],
    {
        type: "App",
        func: {
            type: "App",
            func: { type: "Id", name: "add" },
            arg: { type: "Id", name: "x" },
        },
        arg: { type: "Id", name: "x" },
    }
);

function apply1(f, x, k) {
    if (typeof f === 'function') {
        switch (f.length) {
        case 0: throw new Error("zero arity function??");
        case 1: return doTheCall(f, [x], k);
        default: return k(new Closure(f, [x]));
        }
    } else if (f instanceof Closure) {
        const { func, args } = f;
        const newArgs = args.concat([x]);
        if (newArgs.length < func.length) {
            return k(new Closure(func, newArgs));
        } else {
            return doTheCall(func, newArgs, k);
        }
    } else if (f instanceof Lambda) {
        return doTheCall(f, [x], k);
    } else {
        throw new Error("TODO deal with errors: call non-function");
    }
}

// actually execute a multi-arg call; don't create a closure
function doTheCall(func, args, k) {
    if (typeof func === 'function') {
        return k(func.apply(null, args));
    } else if (func instanceof Lambda) {
        const env = makeEnv(func.params, args);

        // TODO avoid using JS stack here...
        return runExpr(func.body, env, k);
        
    } else {
        throw new Error("internal error: doTheCall got a non-function");
    }
}

function makeEnv(params, args) {
    const env = {};
    for (let i=0; i<params.length; ++i) {
        env[params[i]] = args[i];
    }
    return env;
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
