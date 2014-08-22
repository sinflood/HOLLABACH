%{ open Ast %}
%{ open Printf %}
%{ open Parsing %}
%{ open Lexing %}
%token SEMI LBRACK RBRACK LBRACE RBRACE COMMA LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE LOOP
%token INST BPM TIMESIG
%token <string> NOTE
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
%left TIMESIG

%start program
%type <Ast.program> program

%%

program:
        | inst { [$1] }
        | program inst { $2 :: $1 }

inst:
        INST ID LBRACE stmt_list RBRACE {printf "here -1";{instStr=$2; body=$4} }

mdecl:
    LBRACK note_list RBRACK    { { id = "none";
                body = List.rev $2; timesig = 4; } }
    | ID ASSIGN LBRACK note_list RBRACK   { { id = $1;
                    body = List.rev $4; timesig = 4; } }
    | LBRACK RBRACK {{ id="none"; body=[]; timesig=4;}}
    | ID ASSIGN LBRACK RBRACK {{id=$1;body=[]; timesig=4}}
    | error             { raise(Failure("Malformed measure" ) );  }

note_list:
       chord { [Chord($1)]}
      | note_list chord { Chord($2) :: $1 } 
      | error             { raise(Failure("Malformed note")) } 
note_plus:
        NOTE { [Note($1)] }
      | note_plus PLUS NOTE { Note($3) :: $1 }
chord:
        note_plus { $1 }   

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt {  $2 :: $1 }
stmt:
        IF  expr LBRACE stmt_list RBRACE %prec NOELSE { If($2,
        $4, []) }
  | IF  expr LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
  { If($2, (List.rev $4), (List.rev $8)) }
  | LOOP LITERAL LBRACE stmt_list RBRACE { Loop(Literal($2), $4) }
  | mdecl                       {  Measure($1) }
  | TIMESIG ASSIGN LITERAL { TimeSig($3)}
  | ID     { Id($1) }

expr:
    LITERAL          { Literal($1) }
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


