
class Expr {}
class Literal extends Expr {
    constructor(value) { super(); this.value = value }
    sketch() { return sketch(this.value); }
}
class App extends Expr {
    constructor(func, arg) { super(); this.func = func; this.arg = arg; }
    sketch() { return "(" + sketch(this.func) + ' ' + sketch(this.arg) + ")"; }
}
class Match extends Expr {
    constructor(scrutinee, cases) { super(); this.scrutinee = scrutinee; this.cases = cases; }
    sketch() { return "(match " + sketch(this.scrutinee) + '{ ' + this.cases.map(sketch).join('; ') + '})'; }
}
class Var extends Expr {
    constructor(name) { super(); this.name = name; }
    sketch() { return this.name; }
}
class Case {
    constructor(pattern, expr) { this.pattern = pattern; this.expr = expr; }
    sketch() { return sketch(this.pattern) + ' => ' + sketch(this.expr); }
}

class Pattern {}
class PLiteral extends Pattern {
    constructor(value) { super(); this.value = value; }
    sketch() { return sketch(this.value); }
}
class PVar extends Pattern {
    constructor(name) { super(); this.name = name; }
    sketch() { return this.name; }
}
class PStruct extends Pattern {
    constructor(name, argPats) { super(); this.name = name; this.argPats = argPats; }
    sketch() { return "(" + this.name + this.argPats.map(p => ' ' + sketch(p)).join('') + ")"; }
}


module.exports = {
    Expr,
    Literal,
    App,
    Match,
    Var,
    Case,
    Pattern,
    PLiteral,
    PVar,
    PStruct,
};
