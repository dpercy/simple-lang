
/*

distinct kinds of values:
 - numbers, strings, booleans
 - struct constructors
 - structs
 - primitive functions
 - toplevel functions
 - closures

operations they need to support:
    apply1 :: Value -> Value -> Either3 Error Value (Expr, Env)

        - primitive functions return a value
        - toplevel functions return an expression+env (substed function body)
        - non-saturated things return a new closure
        - structs and atoms error

closures complicate things so maybe use this representation:
- raw JS numbers, strings, booleans
- PrimClosure wraps a JS function and some args
- CodeClosure wraps a piece of code and some args
- StructClosure has a tag and some args
    - can have 0 arity
  - OR a PrimClosure could contain the nary struct-constructor

*/

function apply1(f, x) {
    if ('apply1' in f) {
        return f.apply1(x);
    } else {
        return ['error', 'cannot apply a non-function'];
    }
}

class PrimClosure {
    constructor(func, ...args) {
        this._func = func;
        this._args = args;
        this._arity = func.length - args.length;

        // caller is responsible to not do this
        if (this._arity <= 0) {
            throw Error('prim closure arity');
        }
    }

    apply1(arg) {
        if (this._arity === 1) {
            return ['value', (1, this._func)(...this._args, arg)];
        } else {
            return ['value', new PrimClosure(this._func, ...this._args, arg)];
        }
    }
}

class CodeClosure {
    constructor(params, body, ...args) {
        this._params = params;
        this._body = body;
        this._args = args;
        this._arity = params.length - args.length;

        // caller is responsible to not do this
        if (this._arity <= 0) {
            throw Error('code closure arity');
        }
    }

    apply1(arg) {
        if (this._arity === 1) {
            // return a substed function body
            const env = {};
            for (let i=0; i<this._args.length; ++i) {
                env[this._params[i]] = this._args[i];
            }
            env[this._params[this._params.length-1]] = arg;

            return ['expr+env', {
                expr: this._body,
                env: env,
            }];
        } else {
            // return a new closure
            return ['value', new CodeClosure(this._params, this._body, ...this._args, arg)];
        }
    }
}


class Expr {}
class Literal extends Expr {
    constructor(value) { super(); this.value = value }
}
class App extends Expr {
    constructor(func, arg) { super(); this.func = func; this.arg = arg; }
}
class Match extends Expr {
    constructor(scrutinee, cases) { super(); this.scrutinee = scrutinee; this.cases = cases; }
}
class Var extends Expr {
    constructor(name) { super(); this.name = name; }
}
class Case {
    constructor(pattern, expr) { this.pattern = pattern; this.expr = expr; }
}


/*
App1 Expr Env
App0 Value
Match0 Cases Env
*/
class Frame {}
class App1 extends Frame {
    constructor(funcExpr, env) { super(); this.funcExpr = funcExpr; this.env = env; }
}
class App0 extends Frame {
    constructor(argValue) { super(); this.argValue = argValue; }
}
class Match0 extends Frame {
    constructor(cases, env) { super(); this.cases = cases; this.env = env; }
}


class State {}
class ExprState extends State {
    constructor(currentExpr, env, stack, globals) {
        super();
        this.currentExpr = currentExpr;
        this.env = env;
        this.stack = stack;
        this.globals = globals;
    }

    step() {
        switch (this.currentExpr.constructor) {
            case Literal: return new ValueState(this.currentExpr.value, this.stack, this.globals);
            case Var: return new ValueState(envLookup(this.currentExpr.name, this.env, this.globals), this.stack, this.globals);
            case App: return new ExprState(this.currentExpr.arg, this.env, [new App1(this.currentExpr.func, this.env)].concat(this.stack), this.globals);
            case Match: return new ExprState(this.currentExpr.scrutinee, this.env, [new Match0(this.currentExpr.cases, this.env)].concat(this.stack), this.globals);
            default: throw "no case";
        }
    }
}
class ValueState extends State {
    constructor(currentValue, stack, globals) {
        super();
        this.currentValue = currentValue;
        this.stack = stack;
        this.globals = globals;
    }

    step() {
        if (this.stack.length === 0) return "DONE";

        var frame = this.stack[0];
        var stack = this.stack.slice(1);
        switch (frame.constructor) {
            case App1: return new ExprState(frame.funcExpr, frame.env, [new App0(this.currentValue)].concat(stack), this.globals);
            case App0: {
                var func = this.currentValue;
                var arg = frame.argValue;
                var [tag, newThing] = apply1(func, arg);
                switch (tag) {
                case 'value': return new ValueState(newThing, stack, this.globals);
                case 'expr+env': return new ExprState(newThing.expr, newThing.env, stack, this.globals);
                case 'error': throw "TODO transition to error state";
                default: throw 'bad tag';
                }
            }; throw "fell out";
            case Match0: throw 'TODO match';
            default: throw "no case";
        }
    }
}

function newMachine(expr) {
    return new ExprState(expr, {}, [], {});
}

function hasattr(obj, name) {
    return Object.prototype.hasOwnProperty.call(obj, name);
}
function envLookup(name, env, globals) {
    if (hasattr(env, name)) return env[name];
    if (hasattr(globals, name)) return globals[name];
    throw ('unbound: ' + name);
}


if (require.main === module) {
    console.log('hi!');


    var mul = new PrimClosure((x, y) => x * y);
    var square = new CodeClosure(['x'], new App(new App(new Var('mul'), new Var('x')), new Var('x')));
    var square3 = new App(new Var('square'), new Literal(3));
    var state = new ExprState(square3, {}, [], { square, mul });
    for (;;) {
        console.log(state);
        var newState = state.step();
        if (newState === 'DONE')
            break;
        state = newState;
    }
}
