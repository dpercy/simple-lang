// parcel compiles generators using a polyfill
// (instead of just relying on browser support...)
require('babel-polyfill');

const debounce = require('debounce');
const { parser, runProgram, sketch, PrimClosure } = require('./main');

const CodeMirror = require('./vendor/codemirror');


const editorTextArea = document.getElementById('editor');
const editor = CodeMirror.fromTextArea(editorTextArea, {
    lineNumbers: true,
    lineWrapping: false,
});
window.editor = editor;
editor.focus();

const resultWidgets = [];

function invalidateResultWidgets() {
    const ws = resultWidgets.splice(0);
    for (const w of ws) {
        // Instead this could gray them out or something?
        w.parentElement.removeChild(w);
    }
}
function addResultWidget(pos, text, className) {
    const n = document.createElement('div');
    n.className = className;
    n.innerText = text;
    editor.addWidget(pos, n)
    resultWidgets.push(n);
}

function editorChanged() {
    invalidateResultWidgets();

    const text = editor.getValue();
    console.log('source:', text);

    let program;
    try {
        program = parser.parse(text);
    } catch(e) {
        console.error('parse error:', e);
        return;
    }

    //try {
        const results = runProgram(program, {
            add: new PrimClosure(function add(x, y) { return x + y; }),
            // TODO need to return a struct here, not a boolean...
            lt: new PrimClosure(function lt(x, y) { return x < y; }),
        });
        for (const [stmt, value] of results) {
            console.log(stmt.location, ':', stmt.name || sketch(stmt), '=', sketch(value));
            // stmt.location.{start,end}.{line,column} are both 1-indexed
            const { line, column } = stmt.location.end;
            addResultWidget({ line: line-1, ch: column-1 }, sketch(value), 'result-value');
            //addResultWidget(
        }
    //} catch(e) {
    //    console.error('evaluation error:', e);
    //    return;
    //}
}
editorChanged = debounce(editorChanged, 200);

editor.on('keyup', editorChanged);
editor.on('change', editorChanged);
