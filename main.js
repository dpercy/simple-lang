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
    const emptyArgs = [];
    const emptyEnv = {};
    const emptyK = (v => v);
    return program.map(expr => runExpr(expr, emptyArgs, emptyEnv, emptyK));
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

function runExpr(expr, args, env, k) {
    switch (expr.type) {
    case "Num": {
        const val = +expr.literal;
        return applyValArgs(val, args, k);
    }
    case "App": {
        const { func, arg } = expr;
        const argK = (argV) => {
            return runExpr(func, [argV, ...args], env, k);
        };
        return runExpr(arg, [], env, argK);
    };
    case "Id": {
        const val = lookup(expr.name, env);
        return applyValArgs(val, args, k);
    }
    default: throw new Error("bad expr type: " + expr.type);
    }
}
function applyValArgs(val, args, k) {
    if (args.length === 0) {
            // just a lookup; not actually an application
        return k(val);
    } else if (typeof val === 'function') {
        if (val.length > args.length) {
            return k(new Closure(val, args));
        } else {
            const enoughArgs = args.slice(0, val.length);
            const extraArgs  = args.slice(val.length);
            return applyValArgs(val.apply(null, enoughArgs), extraArgs, k);
        }
    } else if (val instanceof Lambda) {
        if (val.params.length > args.length) {
            return k(new Closure(val, args));
        } else {
            const enoughArgs = args.slice(0, val.params.length);
            const extraArgs  = args.slice(val.params.length);
            const env = {};
            for (let i=0; i<val.params.length; ++i) {
                env[val.params[i]] = enoughArgs[i];
            }
            return runExpr(val.body, extraArgs, env, k);
        }
    } else if (val instanceof Closure) {
        return applyValArgs(val.func, val.args.concat(args), k);
    } else {
        throw new Error("TODO handle errors: apply non-function: " + show(val));
    }
}

function lookup(name, env) {
    switch (name) {
    case "add": return add;
    case "double": return exampleLambdaDouble;
    case "twice": return exampleLambdaTwice;
    default: {
        if (Object.hasOwnProperty.call(env, name)) {
            return env[name];
        } else {
            throw new Error("unbound Id: " + name);
        }
    }
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
exampleLambdaTwice = new Lambda(
    "twice",
    ["f", "x"],
    {
        type: "App",
        func: { type: "Id", name: "f" },
        arg: {
            type: "App",
            func: { type: "Id", name: "f" },
            arg: { type: "Id", name: "x" },
        },
    }
);

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
