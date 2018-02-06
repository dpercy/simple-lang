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


const modRecords = {}; // module-name -> module-record


// load a module and all of its deps into the cache, then return it.
function loadModule(path) {
    // memo and cycle detection
    switch (modRecords[path]) {
        case undefined: break;
        case 'loading': throw "Cyclic dependency on " + path;
        default: return modRecords[path];
    }
    modRecords[path] = 'loading';

    // actually load it
    const modText = fs.readFileSync(path + '.js').toString();
    const modRecord = eval(modText);

    // and its deps
    for (const dep of modRecord.deps) {
        loadModule(dep);
    }

    // memo and cycle detection
    modRecords[path] = modRecord;

    // done!
    return modRecord;
}



const modValues = {}; // module-name -> module-value

// recursively run all the module's deps, then run the module itself.
// loadModule already did cycle detection.
function runModule(path) {
    // memo
    if (path in modValues) return modValues[path];

    // load
    const modRecord = loadModule(path);

    // run deps
    const depValues = modRecord.deps.map(runModule);

    // run itself
    const modValue = modRecord.run(...depValues);

    // memo
    modValues[path] = modValue;

    // done!
    return modValue;
}


function main() {
    const [_node, _rts, arg] = process.argv;
    runModule(arg);
}

// hax preload primitives
const prims = require('./primitives');
modRecords['primitives'] = {
    deps: [],
    run: () => prims,
};

module.exports = {
    loadModule,
    runModule,
};

if (require.main === module) {
    main();
}
