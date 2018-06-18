const fs = require('fs');
const ast = require('./ast');
global.ast = ast;

const peg = require('pegjs');
const grammar = fs.readFileSync('grammar.pegjs').toString();
const parser = peg.generate(grammar);

const { PrimClosure, CodeClosure, ExprState } = require('./cek');

const App = ast.App;
const Var = ast.Var;

function runExprToFinalState(expr) {
    const globals = {
        double: new CodeClosure(['x'], new App(new App(new Var('add'), new Var('x')),
                                               new Var('x'))),
        twice: new CodeClosure(['f', 'x'], new App(new Var('f'),
                                                   new App(new Var('f'),
                                                           new Var('x')))),
        add: new PrimClosure(function add(x, y) { return x + y; })
    };
    
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

if (require.main === module) {
    const [_node, _main, ...args] = process.argv;

    console.log(args);
    
    for (const filename of args) {
        const text = fs.readFileSync(filename).toString();
        const program = parser.parse(text);
        for (const stmt of program) {
            console.log(runExprToFinalState(stmt).sketch());
        }
    }
}
