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
    return program.map(expr => compileExpr(expr)(emptyEnv, emptyK));
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
        this.compiledBody = compileExpr(body);
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

function compileExpr(expr) {
    switch (expr.type) {
    case "Num": {
        const val = +expr.literal;
        return function(env, k) {
            return k(val);
        };
    }
    case "Id": {
        const name = expr.name;
        return function(env, k) {
            const val = lookup(name, env);
            return k(val);
        }
    }
    case "App": {
        const func = compileExpr(expr.func);
        const arg = compileExpr(expr.arg);
        return function(env, k) {
            return arg(env, (argV) => {
                return func(env, (funcV) => {
                    return apply(funcV, [argV], k);
                });
            });
        };
    }
    default:
        throw new Error("bad expr type: " + expr.type);
    }
}
function apply(func, args, k) {
    if (args.length === 0) {
        // fold this case into function and Lambda cases?
        return k(func);
    } else if (func instanceof Closure) {
        return apply(func.func, func.args.concat(args), k);
    } else if (typeof func === 'function') {
        if (args.length >= func.length) {
            const enoughArgs = args.slice(0, func.length);
            const extraArgs = args.slice(func.length);
            const result = func.apply(null, enoughArgs);
            return apply(result, extraArgs, k);
        } else {
            return k(new Closure(func, args));
        }
    } else if (func instanceof Lambda) {
        if (args.length >= func.params.length) {
            const enoughArgs = args.slice(0, func.params.length);
            const extraArgs = args.slice(func.params.length);
            const env = makeEnv(func.params, enoughArgs);
            return func.compiledBody(env, (result) => {
                return apply(result, extraArgs, k);
            });
        } else {
            return k(new Closure(func, args));
        }
    } else {
        throw new Error("TODO handle errors: apply non-function");
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
function makeEnv(params, args) {
    const env = {};
    for (let i=0; i<params.length; ++i) {
        env[params[i]] = args[i];
    }
    return env;
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
