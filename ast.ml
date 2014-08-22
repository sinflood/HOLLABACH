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
        body :  expr list;
        mutable timesig : int;
}
type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | Measure of meas_decl
  | Loop of expr * stmt list
  | TimeSig of int
  | Bpm of int
  | Id of string


type  inst = {
        instStr : string;
        body: stmt list;
}
(*type comp = stmt list*) 
type program = inst list


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
  | Chord(c) -> String.concat "+" (List.map string_of_note c)
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
        md.id ^ "[" ^ String.concat "," (List.map string_of_expr md.body) ^ "]"
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Id(s) -> s
  | TimeSig(m) -> "timesig = " ^ string_of_int m
  | Bpm(b) -> "BPM = " ^ string_of_int b
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Loop(e, s) -> "loop " ^ string_of_expr e ^ " { " ^ String.concat "\n"
  (List.map string_of_stmt s) ^ " }"
  | Measure(m) -> string_of_meas_decl m


let string_of_comp (stmts) = 
        String.concat "" (List.map string_of_stmt stmts.body) ^ "\n"

let string_of_program comps =
  String.concat "\n" (List.map string_of_comp comps)

