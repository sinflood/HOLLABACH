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

%start comp
%type <Ast.comp> comp

%%

comp:
        { Printf.printf "here!" ; [] }  
     | COMP LBRACE stmt_list RBRACE { Printf.printf "here2" ; $3 }
     /*| error             { printf "error here!\n"; [] }*/ 
mdecl:
    LBRACK note_list RBRACK
    { printf "here5"; { id = "none";
                body = List.rev $2; } }
     | error             { printf "error here3!"; {id = "noine"; body=[]} } 

note_list:
      /*  NOTE    { printf "here6"; [ Note($1)] }
      | note_list COMMA NOTE { printf "hereNEW";Note($3) :: $1 } */
      chord { [Chord($1)]}
      | note_list COMMA chord { Chord($3) :: $1 } 
     | error             { printf "error here!5"; [] } 
note_plus:
        NOTE { [Note($1)] }
      /*|  NOTE PLUS NOTE { Note($3) :: [Note($1)]  }*/
      | note_plus PLUS NOTE { Note($3) :: $1 }
chord:
        note_plus { $1 }   

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { printf "here3"; $2 :: $1 }
    /* | error             { printf "error here1!"; [] }*/

stmt:
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | LOOP LITERAL LBRACE stmt RBRACE { Loop(Literal($2), $4) }
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


