%{ open Ast %}
%{ open Printf %}
%{ open Parsing %}
%token SEMI LBRACK RBRACK LBRACE RBRACE COMMA LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT LOOP
%token COMP BPM MEASURELEN
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
%left BPM MEASURELEN

%start program
%type <Ast.program> program

%%

program:
        | comp { [$1] }
        | program comp { $2 :: $1 }

comp:
        COMP ID LBRACE stmt_list RBRACE {{inst=$2; body=$4} }
     /*| error             { printf "error here!\n"; [] }*/ 
mdecl:
    LBRACK note_list RBRACK
    { { id = "none";
    body = List.rev $2; measLen = 4; } }
    | ID ASSIGN LBRACK note_list RBRACK
    { { id = $1;
    body = List.rev $4; measLen = 4; } }
    | LBRACK RBRACK {{ id="none"; body=[]; measLen=4;}}
    | ID ASSIGN LBRACK RBRACK {{id=$1;body=[]; measLen=4}}
     | error             { raise(Failure("Malformed measure")); {id = "none"; body=[];
     measLen = 4;} } 

note_list:
      /*  NOTE    { [ Note($1)] }
      | note_list NOTE { Note($2) :: $1 } */
      | chord { [Chord($1)]}
      | note_list chord { Chord($2) :: $1 } 
     | error             { printf "error here!5"; [] } 
note_plus:
        NOTE { [Note($1)] }
      /*|  NOTE PLUS NOTE { Note($3) :: [Note($1)]  }*/
      | note_plus PLUS NOTE { Note($3) :: $1 }
chord:
        note_plus { $1 }   

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt {  $2 :: $1 }
    /* | error             { printf "error here1!"; [] }*/

stmt:
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | LOOP LITERAL LBRACE stmt_list RBRACE { Loop(Literal($2), $4) }
  | mdecl                       {  Measure($1) }
  | MEASURELEN ASSIGN LITERAL {MeasureLen($3)}
  | BPM ASSIGN LITERAL { Bpm($3) }
  | ID     { Id($1) }
 /*    | error             { printf "error here2!"; Id("a") }*/ 

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


