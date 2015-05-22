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
%token DIVIDE
%token MULTIPLY

%left PLUS MINUS
%left MULTIPLY DIVIDE

%start <Ast.program> prog

%%

prog:
  | f = functions; EOF { Ast.Program(f) }

functions:
  | (* empty *) { [] }
  | f = func; rest = functions { f :: rest }

func:
  DEF; name = ID; LPAREN; args = arguments; RPAREN; LBRACE; body = fbody; RBRACE
  { Ast.Function(Ast.Prototype(name, args), body) };

arguments:
  | { [] }
  | a = arguments_nonzero { a }

arguments_nonzero:
  | id = ID { [id] }
  | id = ID; COMMA; rest = arguments_nonzero { id :: rest }

fbody: e = expr { e }

expr:
  | t1 = expr; PLUS; t2 = expr { Ast.Binary(Ast.Add, t1, t2) }
  | t1 = expr; MINUS; t2 = expr { Ast.Binary(Ast.Subtract, t1, t2) }
  | t1 = expr; MULTIPLY; t2 = expr { Ast.Binary(Ast.Multiply, t1, t2) }
  | t1 = expr; DIVIDE; t2 = expr { Ast.Binary(Ast.Divide, t1, t2) }
  | id = ID; LPAREN; args = expr_arguments; RPAREN { Ast.Call(id, args) }
  | t = term { t }

expr_arguments:
  | { [] }
  | a = expr_arguments_nonzero { a }

expr_arguments_nonzero:
  | e = expr { [e] }
  | e = expr; COMMA; rest = expr_arguments_nonzero { e :: rest }

term:
  | id = ID { Ast.Variable(id) }
  | n = INT { Ast.Number(n) }
