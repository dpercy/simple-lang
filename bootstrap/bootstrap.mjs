
import fs from "fs";
import * as compiler from "./compiler.mjs";

var source_code = fs.readFileSync("/dev/stdin").toString();

// drop #lang line if present
if (source_code.startsWith('#lang ')) {
    source_code = source_code.slice(source_code.indexOf('\n') + 1);
}

const js_code = compiler.$compile_program(source_code);
process.stdout.write(js_code);
