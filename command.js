const fs = require('fs');
const { parser, runProgram, sketch, PrimClosure } = require('./main');

if (require.main === module) {
    const [_node, _main, ...args] = process.argv;

    const primEnv = {
        add: new PrimClosure(function add(x, y) { return x + y; })
    };
    
    for (const filename of args) {
        const readFileSyncAtRunTime = fs.readFileSync;
        const text = readFileSyncAtRunTime(filename).toString();
        const program = parser.parse(text);
        ////console.log('program:', program);
        const results = runProgram(program, primEnv);
        for (const [stmt, value] of results) {
            console.log(stmt.name || sketch(stmt), '=', sketch(value));
        }
    }
}
