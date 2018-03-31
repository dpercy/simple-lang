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

function runExpr(expr) {
    let m = newMachine(expr);
    while (!isFinal(m)) {
        m = step(m);
    }
    return m.expr;
}

function newMachine(expr) {
    return {
        expr: expr,
        isValue: false,
        nargs: 0,
        stack: [],
    };
}

function isFinal(machine) {
    return machine.stack.length === 0 && machine.isValue;
}

function step(machine) {
    if (machine.isValue) {
        // step up
        if (machine.stack.length === 0)
            throw new Error("stack underflow");
        const [frame, ...stack] = machine.stack;
        switch (frame.type) {
        case "FrameFunc": {
            const { nargs, func } = frame;
            const newFrame = {
                type: "FrameArg",
                arg: machine.expr,
            };
            return {
                expr: func,
                isValue: false,
                nargs: nargs + 1,
                stack: [newFrame, ...stack],
            };
        }
        case "FrameArg": {
            throw new Error("what happens here?");
        }
        default: throw new Error("bad frame type: " + frame.type);
        }
    } else {
        // step down
        switch (machine.expr.type) {
        case "Num": return { ...machine, isValue: true };
        case "App": {
            const { nargs, stack, expr: { func, arg } } = machine;
            const newFrame = {
                type: "FrameFunc",
                nargs,
                func,
            };
            return {
                expr: arg,
                isValue: false,
                nargs: 0,
                stack: [newFrame, ...stack],
            };
        }
        case "Id": {
            const { nargs, stack, expr: { name } } = machine;
            // TODO use an env instead
            switch (name) {
            case "add": {
                switch (nargs) {
                case 0: return { ...machine, isValue: true };
                case 1: {
                    // make a closure
                    const [ { arg }, ...rest ] = stack;
                    return {
                        expr: {
                            type: "App",
                            func: machine.expr,
                            arg,
                        },
                        isValue: true,
                        nargs: null,
                        stack: rest,
                    };
                }
                default: {
                    const [ { arg: x }, { arg: y }, ...rest ] = stack;
                    return {
                        expr: doAdd(x, y),
                        isValue: true,
                        nargs: null,
                        stack: rest,
                    };
                }
                }
            }
            default: throw new Error("unbound Id: " + name);
            }
        }
        default: throw new Error("bad expr type: " + machine.expr.type);
        }
    }
}

function doAdd(x, y) {
    if (x.type !== "Num" || y.type !== "Num")
        throw new Error("TODO prim error add non-num");
    // TODO do correct arithmetic
    return {
        type: "Num",
        literal: "" + ((+x.literal) + (+y.literal)),
    };
}


if (require.main === module) {
    const [_node, _main, ...args] = process.argv;

    console.log(args);
    
    for (const filename of args) {
        const text = fs.readFileSync(filename).toString();
        const program = parser.parse(text);
        const result = run(program);
        console.log(result);
    }
}
