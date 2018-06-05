
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

*/

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
