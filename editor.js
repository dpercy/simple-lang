const debounce = require('debounce');
const { parser, runProgram, sketch, PrimClosure } = require('./main');


const editor = document.getElementById('editor');
editor.focus();


function editorChanged() {
    const text = editor.value;
    console.log('source:', text);

    const program = parser.parse(text);
    const results = runProgram(program, {
        add: new PrimClosure(function add(x, y) { return x + y; }),
    });
    for (const [stmt, value] of results) {
        console.log(stmt.location, ':', stmt.name || sketch(stmt), '=', sketch(value));
    }
}
editorChanged = debounce(editorChanged, 200);

editor.addEventListener('keyup', editorChanged);
editor.addEventListener('change', editorChanged);
