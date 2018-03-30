const fs = require('fs');
const peg = require('pegjs');
const grammar = fs.readFileSync('grammar.pegjs').toString();
const parser = peg.generate(grammar);







if (require.main === module) {
    const [_node, _main, ...args] = process.argv;

    console.log(args);
    
    for (const filename of args) {
        const contents = fs.readFileSync(filename).toString();
        const ast = parser.parse(contents);
        console.log(ast);
    }
}
