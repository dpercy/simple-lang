{
  var {
    App,
    Case,
    Literal,
    Match,
    PStruct,
    Var,
  } = global.ast;

}


Program = _line stmts:(Stmt _line)* { return stmts.map(x=>x[0]) }

Stmt = Equation
     / Expression
     
Equation = head:Id args:(_ Id)* _ "=" _ body:Expression {
  args = args.map(x => x[1]); // drop internal whitespace
  throw "TODO defs"
  return {
    type: "Def",
    name: head,
    args: args,
    body: body,
  };
}

Expression = Match / Call

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
PatHole = name:Id { return { type: "PatHole", name: name } }
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
