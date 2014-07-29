%{ open Ast %}

%token SEMI LBRACK RBRACK LBRACE RBRACE COMMA LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT LOOP
%token COMP BPM MEASURELEN
%token <note> NOTE
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left BPM MEASURELEN

%start comp
%type <Ast.comp> comp

%%

comp:
      COMP LBRACE stmt_list RBRACE { Comp($3) }

mdecl:
    ID LBRACK note_list RBRACK
        { { id = $1;
                body = List.rev $3; } }
     | LBRACK note_list RBRACK
        {{body = List.rev $2; }}

note_list:
        NOTE    {[$1]}
      | chord {[$1]}
      | note_list COMMA NOTE { $3 :: $1 }
      | note_list COMMA chord {$3 :: $1 }
        
chord:
        NOTE PLUS NOTE { [$1] }
      | chord PLUS NOTE { $3 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | LOOP LITERAL LBRACE expr RBRACE { Loop($2, $4) }
  | mdecl                       { Measure($1) }
  | MEASURELEN ASSIGN LITERAL {MeasureLen($3)}
  | BPM ASSIGN LITERAL { Bpm($3) }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }


