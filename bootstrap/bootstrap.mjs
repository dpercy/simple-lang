
import fs from "fs";
import * as compiler from "./compiler.mjs";

const source_code = fs.readFileSync("/dev/stdin").toString();
const js_code = compiler.$compile_program(source_code);
console.log(js_code);
