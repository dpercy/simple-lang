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

    // TODO how does this 'breakpoints' thing work?
    gutters: ["CodeMirror-linenumbers", "breakpoints"],
});
window.editor = editor;
editor.focus();

const resultWidgets = [];
const errorMarkers = [];

function invalidateResultWidgets() {
    const ws = resultWidgets.splice(0);
    for (const w of ws) {
        // Instead this could gray them out or something?
        w.parentElement.removeChild(w);
    }
}
function invalidateErrorMarkers() {
    const ms = errorMarkers.splice(0);
    for (const m of ms) {
        m.clear();
    }
}

function addResultWidget(pos, text, className) {
    const n = document.createElement('div');
    n.className = className;
    n.innerText = text;

    const w = document.createElement('div');
    w.appendChild(n);
    w.className = 'result-wrapper';
    editor.addWidget(pos, w)
    resultWidgets.push(w);
}

// Get a CodeMirror position from a PegJS position
function cmFromPJ({ line, column }) {
    return { line: line-1, ch: column-1 };
}

function editorChanged() {
    saveEditorLocally();

    invalidateResultWidgets();
    invalidateErrorMarkers();

    const text = editor.getValue();
    console.log('source:', text);

    let program;
    try {
        program = parser.parse(text);
    } catch(e) {
        console.error('parse error:', e);
        addResultWidget(cmFromPJ(e.location.end), e.message, 'result-widget-error');
        const m = editor.markText(cmFromPJ(e.location.start),
                                  cmFromPJ(e.location.end),
                                  { className: 'mark-error' });
        errorMarkers.push(m);
        return;
    }

    const results = runProgram(program, {
        add: new PrimClosure(function add(x, y) { return x + y; }),
        // TODO need to return a struct here, not a boolean...
        lt: new PrimClosure(function lt(x, y) { return x < y; }),
    });
    for (const [stmt, value, error] of results) {
        const { line, column } = stmt.location.end;
        if (error == null) {
            // stmt.location.{start,end}.{line,column} are both 1-indexed
            addResultWidget({ line: line-1, ch: column-1 }, sketch(value), 'result-widget-value');
        } else {
            addResultWidget({ line: line-1, ch: column-1 }, error, 'result-widget-error');
        }
    }
}
editorChanged = debounce(editorChanged, 200);

editor.on('keyup', editorChanged);
editor.on('change', editorChanged);

function saveEditorLocally() {
    localStorage.editorContents = editor.getValue();
}
function restoreEditor() {
    editor.setValue(localStorage.editorContents);
}

editor.on("gutterClick", function(cm, n) {
    var info = editor.lineInfo(n);
    editor.setGutterMarker(n, "breakpoints", info.gutterMarkers ? null : makeMarker());
});

function makeMarker() {
    var marker = document.createElement("div");
    marker.style.color = "#822";
    marker.innerHTML = "●";
    marker.className = 'breakpoint';
    return marker;
}


restoreEditor();
