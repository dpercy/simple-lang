
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

function sketch(v) {
    if (Array.isArray(v)) {
        return '[' + v.map(sketch).join(', ') + ']';
    } else if (typeof v === 'object' && 'sketch' in v) {
        return v.sketch();
    } else {
        return JSON.stringify(v);
    }
}

function sketchEnv(e) {
    return Object.entries(e).map(([k, v]) => k + '=' + sketch(v)).join(' ')
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

    sketch() { return "(" + this._func.name + this._args.map(a => ' ' + sketch(a)).join('') + ")"; }

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
        this._arityRemaining = params.length - args.length;

        // caller is responsible to not do this
        if (this._arityRemaining <= 0) {
            throw Error('code closure arity');
        }
    }

    apply1(arg) {
        if (this._arityRemaining === 1) {
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

class StructClosure {
    constructor(name, arityTotal, ...args) {
        this._name = name;
        this._arityTotal = arityTotal;
        this._args = args;

        if (args.length > this._arityTotal) {
            throw Error('struct closure arity');
        }
    }

    sketch() { return "(" + this._name + this._args.map(a => ' '+sketch(a)).join('') + ")"; }

    apply1(arg) {
        if (this._args.length === this._arityTotal) {
            return ['error', 'cannot apply a struct'];
        }
        return ['value', new StructClosure(this._name, this._arityTotal, ...this._args, arg)];
    }

    // treat this value as a pattern:
    // return either null for no match, or an array of argument scrutinees.
    unapply(scrutinee) {
        if (this._args.length > 0) {
            throw "can't use a curried constructor as a pattern";
        }

        // is it a struct? (as opposed to a number, or a curried constructor)
        if (!(scrutinee instanceof StructClosure))
            return null;
        if (scrutinee._arityTotal !== scrutinee._args.length)
            return null;

        // is it the right kind of struct?
        if (scrutinee._name !== this._name)
            return null;
        if (scrutinee._arityTotal !== this._arityTotal)
            throw "How did the names match but not the arities???"

        // then get the fields!
        return scrutinee._args;
    }
}


class Expr {}
class Literal extends Expr {
    constructor(value) { super(); this.value = value }
    sketch() { return sketch(this.value); }
}
class App extends Expr {
    constructor(func, arg) { super(); this.func = func; this.arg = arg; }
    sketch() { return "(" + sketch(this.func) + ' ' + sketch(this.arg) + ")"; }
}
class Match extends Expr {
    constructor(scrutinee, cases) { super(); this.scrutinee = scrutinee; this.cases = cases; }
    sketch() { return "(match " + sketch(this.scrutinee) + '{ ' + this.cases.map(sketch).join('; ') + '})'; }
}
class Var extends Expr {
    constructor(name) { super(); this.name = name; }
    sketch() { return this.name; }
}
class Case {
    constructor(pattern, expr) { this.pattern = pattern; this.expr = expr; }
    sketch() { return sketch(this.pattern) + ' => ' + sketch(this.expr); }
}

class Pattern {}
class PLiteral extends Pattern {
    constructor(value) { super(); this.value = value; }
    sketch() { return sketch(this.value); }
}
class PVar extends Pattern {
    constructor(name) { super(); this.name = name; }
    sketch() { return this.name; }
}
class PStruct extends Pattern {
    constructor(name, argPats) { super(); this.name = name; this.argPats = argPats; }
    sketch() { return "(" + this.name + this.argPats.map(p => ' ' + sketch(p)).join('') + ")"; }
}

/*
App1 Expr Env
App0 Value
Match0 Cases Env
*/
class Frame {}
class App1 extends Frame {
    constructor(funcExpr, env) { super(); this.funcExpr = funcExpr; this.env = env; }
    sketch() { return "(" + sketch(this.funcExpr) + " _)"; }
}
class App0 extends Frame {
    constructor(argValue) { super(); this.argValue = argValue; }
    sketch() { return "(_ " + sketch(this.argValue) + ")"; }
}
class Match0 extends Frame {
    constructor(cases, env) { super(); this.cases = cases; this.env = env; }
    sketch() { return "(match _ { ... })"; }
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

    sketch() {
        return "> " + sketch(this.currentExpr) + '\t' + sketchEnv(this.env) + '\t' + sketch(this.stack);
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
            case Match0: {
                var scrut = this.currentValue;
                var cases = frame.cases;
                for (var case_ of cases) {
                    var matchVars = tryMatch(case_.pattern, scrut, this.globals);
                    if (matchVars) {
                        return new ExprState(case_.expr, { ...frame.env, ...matchVars }, stack, this.globals);
                    }
                }
                throw "TODO transition to no-match-case error";
            }; throw "fell out";
            default: throw "no case";
        }
    }

    sketch() {
        return "< " + sketch(this.currentValue) + '\t' + sketch(this.stack);
    }
}

function tryMatch(pattern, value, globals) {
    switch (pattern.constructor) {
    case PLiteral: return (value === pattern.value) ? {} : null;
    case PVar: return { [pattern.name]: value };
    case PStruct: {
        var unfunc = envLookup(pattern.name, {}, globals);
        var maybeArgs = unfunc.unapply(value);
        if (maybeArgs === null)
            return null;

        var result = {};
        for (var i=0; i<maybeArgs.length; ++i) {
            var arg = maybeArgs[i];
            var pat = pattern.argPats[i];
            var subResult = tryMatch(pat, arg, globals);
            if (subResult === null)
                return null;
            Object.assign(result, subResult);
        }
        return result;
    }; throw "fell off";
    default: throw "no case";
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
    // TODO transition to error state instead of assertion error here?
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

    console.log('');
    console.log('');
    console.log('');
    var binop = (funcName, a, b) => new App(new App(new Var(funcName), a), b);
    var lt = new PrimClosure((x, y) => x < y);
    var add = new PrimClosure((x, y) => x + y);
    var sub = new PrimClosure((x, y) => x - y);
    var fib = new CodeClosure(['n'], new Match(binop('lt', new Var('n'), new Literal(2)), [
        new Case(new PLiteral(true), new Var('n')),
        new Case(new PLiteral(false), binop('add',
                                            new App(new Var('fib'), binop('sub', new Var('n'), new Literal(1))),
                                            new App(new Var('fib'), binop('sub', new Var('n'), new Literal(2))))),
    ]));
    var main = new App(new Var('fib'), new Literal(6));
    var state = new ExprState(main, {}, [], { add, lt, fib, sub });
    for (;;) {
        console.log(state);
        var newState = state.step();
        if (newState === 'DONE')
            break;
        state = newState;
    }


    console.log('');
    console.log('');
    console.log('');

    var revappend = new CodeClosure(['xs', 'ys'], new Match(new Var('xs'), [
        new Case(new PStruct('Empty', []), new Var('ys')),
        new Case(new PStruct('Cons', [new PVar('x'), new PVar('xs')]),
                 binop('revappend', new Var('xs'), binop('Cons', new Var('x'), new Var('ys')))),
    ]));

    var Empty = new StructClosure('Empty', 0);
    var Cons = new StructClosure('Cons', 2);

    var main = binop('revappend',
                     binop('Cons', new Literal(2), binop('Cons', new Literal(1), new Var('Empty'))),
                     binop('Cons', new Literal(3), new Var('Empty')));
    var state = new ExprState(main, {}, [], { Empty, Cons, revappend });
    for (;;) {
        console.log(sketch(state));
        console.log('');
        var newState = state.step();
        if (newState === 'DONE')
            break;
        state = newState;
    }

}
