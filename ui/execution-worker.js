/*

This web worker compiles and runs a single program.

*/



/*
  TODO what about this?
configureRuntime({
    toplevelPrinter: function(name, lineno, val) {
        const msg = name ? "(def " + name + " " + show(val) + ")" : show(val);

        const div = document.createElement('div');
        div.innerText = msg;
        div.classList = 'result value';
        
        editor.addLineWidget(lineno, div);
    },
    errorPrinter: function(name, lineno, e) {
        const msg = "(error " + show(e) + ")";
        
        const div = document.createElement('div');
        div.innerText = msg;
        div.classList = 'result error';

        editor.addLineWidget(lineno, div);
    },
});
*/

importScripts('./bootstrap/BigInteger.js');
importScripts('./bootstrap/primitives.js');

class RTS {
    constructor(prefix) {
        this.prefix = prefix;
        this.modRecords = {
            primitives: {
                deps: [],
                run: function() {
                    return primitives;
                },
            }
        };
        this.modValues = {};
    }

    // loads the given module and its deps
    async loadModule(modname) {
        // memo and cycle detection
        switch (this.modRecords[modname]) {
        case undefined: break;
        case 'loading': throw Error("import cycle in " + modname);
        default: return this.modRecords[modname];
        }

        // fetch it
        const url = this.prefix + modname + '.js'
        const response = await fetch(url);
        const text = await response.text();
        const record = eval(text);

        // recursively fetch all its deps
        // TODO promise.all?
        for (const dep of record.deps) {
            await this.loadModule(dep);
        }

        // memo and cycle detection
        this.modRecords[modname] = record;

        // done!
        return record;
    }

    // NOTE! this assumes you've already loaded the module.
    runModule(modname) {
        // memo
        if (modname in this.modValues) return this.modValues[modname];

        // check our assumption
        const record = this.modRecords[modname];
        if (!record) throw Error("module not loaded: " + modname);

        // run deps
        const depValues = record.deps.map(dep => this.runModule(dep));

        // run itself
        const value = record.run(...depValues);

        // memo
        this.modValues[modname] = value;

        // done!
        return value;
    }
}

async function getCompiler() {
    const rts = new RTS('bootstrap/');
    await rts.loadModule('compiler');
    return rts.runModule('compiler');
}
const compilerPromise = getCompiler();


onmessage = async function(e) {
    const compiler = await compilerPromise;
    const sourceText = e.data;

    const jsText = compiler.$compile_program(sourceText);

    // for now let's just compile, not run
    postMessage(jsText);

    close();
};
   
