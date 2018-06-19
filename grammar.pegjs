{
  var {
    Var,
    App,
    Case,
    Literal,
    
    Match,
    PStruct,
    PVar,

    DefVal,
    DefStruct,
    DefFunc,
  } = global.ast;

  function loc(v) {
    v.location = location();
    return v;
  }

}


Program = _line stmts:(Stmt _line)* { return stmts.map(x=>x[0]) }

Stmt = v:( Equation
         / StructDefinition
         / Expression) {
  return loc(v)
}
     
Equation = head:Id args:(_ Id)* _ "=" _ body:Expression {
  args = args.map(x => x[1]); // drop internal whitespace
  if (args.length === 0) {
    return new DefVal(head, body);
  } else {
    return new DefFunc(head, args, body);
  }
}

StructDefinition = "struct" _ name:Ctor _ arity:Num {
  return new DefStruct(name, +arity);
}

Expression = v:(Match / Call) {
  return loc(v)
}

Match = "match" _ scrut:Expression _ "{" _line cases:(Case _line)* "}" {
  return new Match(
    scrut,
    cases.map(x=>x[0]),
  )
}

Case = lhs:Pattern _ "->" (_line) rhs:Expression {
  return new Case(lhs, rhs)
}

Pattern = PatCall
PatCall = PatHole / PatCtor
PatArg = PatHole / "(" _ c:PatCall _ ")" { return c }
PatHole = name:Id { return new PVar(name) }
PatCtor = name:Ctor args:(_ PatArg)* {
  return new PStruct(name, args.map(x=>x[1]))
}

Call = f:Arg xs:(_ Arg)* {
  function app(f, x) {
    return new App(f, x);
  }
  xs = xs.map(x=>x[1]); // select the Arg part
  return xs.reduce(app, f);
}

Arg = name:Id { return new Var(name) }
    / name:Ctor { return new Var(name) }
    / num:Num { return new Literal(+num) }
    / "(" _ e:Expression _ ")" { return e }

// TODO newlines within an expression? in a call?
_ "whitespace" = [ \t\r]*
_line "newline" = [ \t\r\n]*

Id "identifier" = first:[a-z] rest:[a-zA-Z_0-9]* { return text() }
Ctor "constructor" = first:[A-Z] rest:[a-zA-Z_0-9]* { return text() }

Num "number" = "-"? [0-9]+ { return text(); }
