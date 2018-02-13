/*

This web worker compiles and runs a single program.

*/



/*
  TODO what about this?
;
*/

importScripts('./bootstrap/BigInteger.js');
importScripts('./bootstrap/primitives.js');

class RTS {
    constructor(prefix) {
        this.prefix = prefix;
        this.modRecords = {};
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

    // TODO could break this up to send "parse" events,
    // so the UI can mark each statement with a throbber,
    // or maybe leave the previous result showing but grayed out
    let jsText;
    try {
        jsText = compiler.$compile_program(sourceText);
    } catch(e) {
        const {  message, stack } = e;
        postMessage({
            type: 'compileFailed',
            message: e.toString(),
            stack: e.stack,
        });
        close();
        return;
    }
    postMessage({ type: 'compiled', jsText });

    // set up runtime system
    const rts = new RTS('bootstrap/');
    await rts.loadModule('primitives');
    const primitives = rts.runModule('primitives');


    // load the "standard library"
    await rts.loadModule('bool');
    await rts.loadModule('int');
    await rts.loadModule('string');
    await rts.loadModule('list');
    // hack: run the standard library before configureRuntime,
    // so it doesn't print on the page.
    rts.runModule('bool');
    rts.runModule('int');
    rts.runModule('string');
    rts.runModule('list');
    
    primitives.configureRuntime({
        toplevelPrinter: function(name, lineno, val) {
            const repr = primitives.show(val);
            self.postMessage({ type: 'statementValue', name, lineno, repr });
        },
        errorPrinter: function(name, lineno, err) {
            const repr = primitives.show(err);
            self.postMessage({ type: 'statementError', name, lineno, repr });
        },
    });

    
    // hack: prevent further fetching
    rts.prefix = 'http://about:blank/';
    
    rts.modRecords['main'] = eval(jsText);
    rts.runModule('main');

    close();
};
   