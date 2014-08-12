open Lexing
open Parsing
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type note =
        Note of string
type expr =
    Literal of int
  | Chord of note list
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type meas_decl = {
        id : string;
        body :  note list;
}
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Measure of meas_decl
  | Loop of expr * stmt
  | MeasureLen of int
  | Bpm of int
  | Id of string

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }


type program = string list * func_decl list
type comp = 
        stmt list


(*exception LexErr of string
exception ParseErr of string

let error msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum 
                      (start.pos_cnum -start.pos_bol) (finish.pos_cnum -
                      finish.pos_bol) msg

let lex_error lexbuf = 
            raise ( LexErr (error (lexeme lexbuf) (lexeme_start_p lexbuf)
            (lexeme_end_p lexbuf)))

let parse_error msg nterm =
            raise ( ParseErr (error msg (rhs_start_pos nterm) (rhs_end_pos nterm)))
*)
let rec string_of_note = function
        Note(n) -> n
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Chord(c) -> String.concat "" (List.map string_of_note c)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let string_of_meas_decl md = 
        md.id ^ "[" ^ String.concat "," (List.map string_of_note md.body) ^ "]"
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Id(s) -> s
  | MeasureLen(m) -> "measureLen = " ^ string_of_int m
  | Bpm(b) -> "BPM = " ^ string_of_int b
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Loop(e, s) -> "loop " ^ string_of_expr e ^ " { " ^ string_of_stmt s ^ " }"
  | Measure(m) -> string_of_meas_decl m

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

let string_of_comp (stmts) = 
        String.concat "" (List.map string_of_stmt stmts) ^ "\n"
