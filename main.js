const fs = require('fs');
const ast = require('./ast');
const { sketch } = require('./sketch');

const parser = require('./grammar.generated');

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
        return runExprToValue(stmt.value, globals);
    default:
        throw "no case for: " + stmt;
    }
}

function* runProgram(stmts, primEnv) {
    // yields a [statement, value, error] triple for each statement in the input.
    // `value` is only valid if `error` is non-null.

    // create an env where each global is "not yet computed"
    var globals = Object.assign({}, primEnv);
    for (const stmt of stmts) {
        if (stmt instanceof Def) {
            globals[stmt.name] = undefined;
        }
    }

    // evaluate each statement in the global env
    for (const stmt of stmts) {
        let val;
        try {
            val = runStmtToValue(stmt, globals);
        } catch(e) {
            yield [stmt, undefined, e];
            continue;
        }

        if (stmt instanceof Def) {
            globals[stmt.name] = val;
        }
        yield [stmt, val, null];
    }
}


module.exports = {
    parser, runProgram, sketch, PrimClosure
};
