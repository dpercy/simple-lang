const fs = require('fs');
const ast = require('./ast');
global.ast = ast;
const { sketch } = require('./sketch');

const peg = require('pegjs');
const grammar = fs.readFileSync('grammar.pegjs').toString();
const parser = peg.generate(grammar);

const {
    PrimClosure,
    CodeClosure,
    StructClosure,
    ExprState,
    ValueState,
} = require('./cek');

const { Expr, App, Var,
        Def, DefFunc, DefVal, DefStruct } = ast;

function runExprToFinalState(expr, globals) {
    const emptyEnv = {};
    const emptyStack = [];
    
    var state = new ExprState(expr, emptyEnv, emptyStack, globals);
    for (;;) {
        var newState = state.step();
        if (newState === 'DONE')
            return state;
        state = newState;
    }
}

function runExprToValue(expr, globals) {
    var finalState = runExprToFinalState(expr, globals);
    if (finalState.constructor !== ValueState)
        throw "done state not a value???";
    return finalState.currentValue;
}

function runStmtToValue(stmt, globals) {
    if (stmt instanceof Expr) {
        return runExprToValue(stmt, globals);
    }

    switch (stmt.constructor) {
    case DefStruct:
        return new StructClosure(stmt.name, stmt.arity);
    case DefFunc:
        return new CodeClosure(stmt.name, stmt.params, stmt.body);
    case DefVal:
        return runExprToValue(stmt.value);
    default:
        throw "no case for: " + stmt;
    }
}

function* runProgram(stmts, primEnv) {
    // yields a [statement, value] pair for each statement in the input.

    // create an env where each global is "not yet computed"
    var globals = { ...(primEnv || {}) };
    for (const stmt of stmts) {
        if (stmt instanceof Def) {
            globals[stmt.name] = undefined;
        }
    }

    // evaluate each statement in the global env
    for (const stmt of stmts) {
        const val = runStmtToValue(stmt, globals);
        if (stmt instanceof Def) {
            globals[stmt.name] = val;
        }
        debugger;
        yield [stmt, val];
    }
}


if (require.main === module) {
    const [_node, _main, ...args] = process.argv;

    const primEnv = {
        add: new PrimClosure(function add(x, y) { return x + y; })
    };
    
    for (const filename of args) {
        const text = fs.readFileSync(filename).toString();
        const program = parser.parse(text);
        ////console.log('program:', program);
        const results = runProgram(program, primEnv);
        for (const [stmt, value] of results) {
            console.log(stmt.name || sketch(stmt), '=', sketch(value));
        }
    }
}
