%token <int> INT
%token <string> ID
%token DEF
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token EOF
%token PLUS
%token MINUS
%start <Ast.program> prog

%%

prog:
  | f = functions; EOF { Ast.Program(f) }

functions:
  | (* empty *) { [] }
  | f = func; rest = functions { f :: rest }

func:
  DEF; name = ID; args = arguments; LBRACE; body = fbody; RBRACE
  { Ast.Function(Ast.Prototype(name, args), body) };

arguments:
  | LPAREN; RPAREN { [] }
  | LPAREN; a = arguments_nonzero; RPAREN { a }

arguments_nonzero:
  | id = ID { [id] }
  | id = ID; COMMA; rest = arguments_nonzero { id :: rest }

fbody: e = expr { e }

expr:
  | t1 = term; PLUS; t2 = expr { Ast.Binary(Ast.Plus, t1, t2) }
  | t1 = term; MINUS; t2 = expr { Ast.Binary(Ast.Minus, t1, t2) }
  | t = term { t }

term:
  | id = ID { Ast.Variable(id) }
  | n = INT { Ast.Number(n) }
