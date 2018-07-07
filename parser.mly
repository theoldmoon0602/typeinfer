%{
  open Syntax
%}

%token EOL FUN ARROW LT GT LPAREN RPAREN
%token TRUE FALSE AND OR
%token PLUS TIMES
%token IF THEN ELSE
%token APP
%token <int> NUMBER
%token <string> ID

%nonassoc LPAREN ID NUMBER TRUE FALSE
%left APP
%left AND OR
%left LT GT
%left PLUS
%left TIMES

%start main
%type<Syntax.expr> main
%%

main:
  expr EOL { $1 }

expr:
  |ID {Var($1)}
  |TRUE {BoolLit(true)}
  |FALSE {BoolLit(false)}
  |NUMBER { NumLit($1) }
  |expr PLUS expr { Binop(Add, $1, $3) }
  |expr TIMES expr { Binop(Mul, $1, $3) }
  |expr LT expr { Binop(Lt, $1, $3) }
  |expr GT expr { Binop(Gt, $1, $3) }
  |expr AND expr { Binop(And, $1, $3) }
  |expr OR expr { Binop(Or, $1, $3) }
  |LPAREN FUN name=ID ARROW body=expr RPAREN { Fun(name, body) }
  |func=expr arg=expr %prec APP  { App(func, arg) }
  |LPAREN expr RPAREN { $2 }
  |IF cond=expr THEN t=expr ELSE e=expr { If(cond,t,e) }

