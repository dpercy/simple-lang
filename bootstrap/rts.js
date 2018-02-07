/*
Synchronously run a module in this format:
;({
    deps: ['foo.js', 'bar.js'],
    run: function(foo, bar) {
        ...
        const x = foo.one(123);
            const y = bar.thing(x);
        ...
        return { x, y };
    },
});
*/


const fs = require('fs');
const path = require('path');

function makeRTS({ basedir }) {

    const modRecords = {}; // module-name -> module-record
    
    // hax preload primitives
    const prims = require('./primitives');
    modRecords['primitives'] = {
        deps: [],
        run: () => prims,
    };

    // load a module and all of its deps into the cache, then return it.
    function loadModule(modname) {
        // memo and cycle detection
        switch (modRecords[modname]) {
        case undefined: break;
        case 'loading': throw "Cyclic dependency on " + modname;
        default: return modRecords[modname];
        }
        modRecords[modname] = 'loading';

        // actually load it
        const filename = path.join(basedir, modname + '.js');
        const modText = fs.readFileSync(filename).toString();
        const modRecord = eval(modText);

        // and its deps
        for (const dep of modRecord.deps) {
            loadModule(dep);
        }

        // memo and cycle detection
        modRecords[modname] = modRecord;

        // done!
        return modRecord;
    }
    
    const modValues = {}; // module-name -> module-value
    
    
    // recursively run all the module's deps, then run the module itself.
    // loadModule already did cycle detection.
    function runModule(modname) {
        // memo
        if (modname in modValues) return modValues[modname];

        // load
        const modRecord = loadModule(modname);

        // run deps
        const depValues = modRecord.deps.map(runModule);

        // run itself
        const modValue = modRecord.run(...depValues);

        // memo
        modValues[modname] = modValue;

        // done!
        return modValue;
    }

    return { loadModule, runModule };
}


function main() {
    const [_node, _rts, filename] = process.argv;

    // thing/foo.js  ->  { dir: 'thing/', name: 'foo', ext: '.js' }
    const { dir, name } = path.parse(filename);

    const rts = makeRTS({
        basedir: dir,
    });

    rts.runModule(name);
}


module.exports = {
    makeRTS,
};

if (require.main === module) {
    main();
}
