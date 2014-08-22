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
  | Noexpr

type meas_decl = {
        id : string;
        body :  expr list;
        mutable timesig : int;
}
type stmt =
  | Expr of expr
  | If of expr * stmt list * stmt list
  | Measure of meas_decl
  | Loop of expr * stmt list
  | TimeSig of int
  | Id of string


type  inst = {
        instStr : string;
        body: stmt list;
}
type program = inst list


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
  | Noexpr -> ""

let string_of_meas_decl md = 
        md.id ^ "[" ^ String.concat "," (List.map string_of_expr md.body) ^ "]"
let rec string_of_stmt = function
   Expr(expr) -> string_of_expr expr ^ ";\n";
  | Id(s) -> s
  | TimeSig(m) -> "timesig = " ^ string_of_int m
(*  | If(e, s, []) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s*)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
           String.concat "\n" (List.map string_of_stmt s1) ^ "else\n" ^
           String.concat "\n" (List.map string_of_stmt s2)
  | Loop(e, s) -> "loop " ^ string_of_expr e ^ " { " ^ String.concat "\n"
  (List.map string_of_stmt s) ^ " }"
  | Measure(m) -> string_of_meas_decl m


let string_of_comp (stmts) = 
        String.concat "" (List.map string_of_stmt stmts.body) ^ "\n"

let string_of_program comps =
  String.concat "\n" (List.map string_of_comp comps)

