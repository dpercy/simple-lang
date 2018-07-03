// parcel compiles generators using a polyfill
// (instead of just relying on browser support...)
require('babel-polyfill');

const debounce = require('debounce');
const { parser, runProgram, sketch, PrimClosure } = require('./main');


const editor = document.getElementById('editor');
editor.focus();


function editorChanged() {
    const text = editor.value;
    console.log('source:', text);

    let program;
    try {
        program = parser.parse(text);
    } catch(e) {
        console.error('parse error:', e);
        return;
    }

    try {
        const results = runProgram(program, {
            add: new PrimClosure(function add(x, y) { return x + y; }),
        });
        for (const [stmt, value] of results) {
            console.log(stmt.location, ':', stmt.name || sketch(stmt), '=', sketch(value));
        }
    } catch(e) {
        console.error('evaluation error:', e);
        return;
    }
}
editorChanged = debounce(editorChanged, 200);

editor.addEventListener('keyup', editorChanged);
editor.addEventListener('change', editorChanged);
