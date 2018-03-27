
Program = _line stmts:(Stmt _line)* { return stmts.map(x=>x[0]) }

Stmt = Equation
     / Expression
     
Equation = head:Id args:(_ Id)* _ "=" _ body:Expression {
  args = args.map(x => x[1]); // drop internal whitespace
  return {
    type: "Def",
    name: head,
    args: args,
    body: body,
  };
}

// TODO match expressions
Expression = Match / Call

Match = "match" _ scrut:Expression _ "{" _line cases:(Case _line)* "}" {
  return {
    type: "Match",
    scrutinee: scrut,
    cases: cases.map(x=>x[0]),
  };
}

// TODO pattern, not expr
Case = lhs:Expression _ "->" (_line) rhs:Expression {
  return {
    type: "Case",
    lhs: lhs,
    rhs: rhs,
  };
}

Call = f:Arg xs:(_ Arg)* {
  function app(f, x) {
    return { type: "App", func: f, arg: x };
  }
  xs = xs.map(x=>x[1]); // select the Arg part
  return xs.reduce(app, f);
}

Arg = name:Id { return { type: "Id", name: name } }
    / num:Num { return { type: "Num", literal: num } }
    / "(" _ e:Expression _ ")" { return e }

// TODO newlines within an expression? in a call?
_ "whitespace" = [ \t\r]*
_line "newline" = [ \t\r\n]*

Id "identifier" = first:[a-zA-Z_] rest:[a-zA-Z_0-9]* {
  return text();
}

Num "number" = "-"? [0-9]+ { return text(); }
