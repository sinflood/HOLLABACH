open Ast
open Printf

module StringMap = Map.Make(String)
let header  = "Timing Resolution (pulses per quarter note)\n4\n\nInstrument,105,Banjo\n\nTick,Note (0-127),Velocity (0-127)\n"

let writeOutput bpm k  = let oc = open_out_gen [Open_creat; Open_wronly;
Open_trunc; Open_text] 0o666 "out.bach" in 
let getVel notestr =
        match notestr.[ (String.length notestr) -1] with
        '1' -> 4
        | '2' ->2 
        | _ -> 1
in
let getMod noter = 
        match noter.[1] with
        'b' -> -1
        | '#' -> 1
        | _ -> 0
in
let getOctave noter =
        printf "char1: %c val: %i char2: %c val %i mod: %i " noter.[1]
        (int_of_char noter.[1]) noter.[2] (int_of_char noter.[2]) (getMod noter);
        if getMod noter != 0 then
                ((int_of_char noter.[2] -48)* 12 + 12) (*octave 0 starts at 24*)
        else
                (int_of_char noter.[1] -48)* 12 + 12
in
let getNote noter =  
        match noter.[0] with
        'A' -> 9 + getOctave noter
        | 'B' -> 11 + getOctave noter
        | 'C' -> 0 + getOctave noter
        | 'D' -> 2 + getOctave noter
        | 'E' -> 4 + getOctave noter
        | 'F' -> 5 + getOctave noter
        | 'G' -> 7 + getOctave noter
        | _ -> printf "Not a note value!"; 0
   
in
let getNoteString no offset = 
         match no with
         Note(n) -> string_of_int offset ^ "," ^ string_of_int ((getNote n) + (getMod n)) ^ "," ^ string_of_int (getVel n)
in
let printNote naw offset=
        match naw with
        Note(n) -> if n.[0] != 'R' then
                fprintf oc "%s\n" (getNoteString naw offset)
in
let printMeasure off n =
        match n with
        Measure(m) -> 
                printf "Len: %i off: %i mlen: %i\n" (List.length m.body) off
                m.measLen;
                let noteOff = (float_of_int m.measLen /. float_of_int
                (List.length m.body)) *. 4.0 in
                printf " NO: %f" noteOff;        
                List.fold_left (fun note_num naw ->
                        match naw with
                        (*Note(n) -> fprintf oc "%s\n" (getNoteString naw (off+note_num)); note_num + noteOff
                        |*) Chord(cr) -> List.iter (fun nat -> printNote nat
                        (off+note_num)) cr; note_num + int_of_float noteOff
        )  0 m.body; off + (m.measLen * 4)
        | _ -> printf "ERROR, NOT A MEASURE!"; off
in
printf "start print\n" ;
fprintf oc "%s" header;
List.fold_left printMeasure 0 k ; 
close_out oc


let  vars = ref StringMap.empty

let compile stmts = 
        Printf.printf "here too\n";
        let rec eval env = match env with
                Literal (l) -> l
               (* | Note(n) -> Note(n), env*)
                | a -> 0 
        in
let print_vars key measure =
            print_string(key ^ " " ^ string_of_meas_decl measure ^ "\n")
in
let currMeasLen = ref 4
in
let currBPM = 120
in
        let rec exec out env = match env with
        Measure(m) -> (vars := StringMap.add m.id m !vars); printf "var:|%s|\n"
        m.id; printf "currML!!!: %i\n" !currMeasLen; m.measLen <- !currMeasLen; Measure(m) :: out
                | MeasureLen(m) -> printf "!!!!!ML:%i\n" m; currMeasLen := m; out
                | Bpm(b) -> currBPM = b; out
                | Loop(c, b) -> let rec callLoop i body = 
                        if i>0 then
                        (List.fold_left exec [] body) @ (callLoop (i-1) body)
                        else [] 
                in (List.rev (callLoop (eval c) b)) @ out (*for l.val: list.fold_left exec l.body l.val :: out (*TODO: add
                currMeasLen*)*)
                | Id(i) -> printf "whole? %s" (string_of_bool (StringMap.mem
                "whole" !vars)); StringMap.iter print_vars !vars; printf "looking
                for: %s\n" i; Measure((StringMap.find i !vars)) :: out (* TODO lookup i *)
                | If(c,b1,b2) -> out
                | a -> out
                (*| Expr(e) -> let _, env = eval env e in env*)
        in
        (*let run s = 
                List.map fst (List.fold_left (fun l x -> (exec x) :: l ) [] s)
        in 
        let res y = 
                List.map fst y 
        in
        let k = run (List.rev stmts)
        in
        let v = res  (run (List.rev stmts))
        in*)
        let comped = List.fold_left exec [] (List.rev stmts)
        in
        writeOutput 120 (List.rev comped) (* TODO why do we need to reverse it?*)
        (*List.iter writeOutput (List.map exec stmts)*) 
(*
(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =

  (* Allocate "addresses" for each global variable *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in

  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and local_offsets = enum 1 1 fdecl.locals
    and formal_offsets = enum (-1) (-2) fdecl.formals in
    let env = { env with local_index = string_map_pairs
		  StringMap.empty (local_offsets @ formal_offsets) } in

    let rec expr = function
	Literal i -> [Lit i]
      | Id s ->
	  (try [Lfp (StringMap.find s env.local_index)]
          with Not_found -> try [Lod (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Assign (s, e) -> expr e @
	  (try [Sfp (StringMap.find s env.local_index)]
  	  with Not_found -> try [Str (StringMap.find s env.global_index)]
	  with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Call (fname, actuals) -> (try
	  (List.concat (List.map expr (List.rev actuals))) @
	  [Jsr (StringMap.find fname env.function_index) ]   
        with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> []

    in let rec stmt = function
	Block sl     ->  List.concat (List.map stmt sl)
      | Expr e       -> expr e @ [Drp]
      | Return e     -> expr e @ [Rts num_formals]
      | If (p, t, f) -> let t' = stmt t and f' = stmt f in
	expr p @ [Beq(2 + List.length t')] @
	t' @ [Bra(1 + List.length f')] @ f'
      | For (e1, e2, e3, b) ->
	  stmt (Block([Expr(e1); While(e2, Block([b; Expr(e3)]))]))
      | While (e, b) ->
	  let b' = stmt b and e' = expr e in
	  [Bra (1+ List.length b')] @ b' @ e' @
	  [Bne (-(List.length b' + List.length e'))]

    in [Ent num_locals] @      (* Entry: allocate space for locals *)
    stmt (Block fdecl.body) @  (* Body *)
    [Lit 0; Rts num_formals]   (* Default = return 0 *)

  in let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
  let entry_function = try
    [Jsr (StringMap.find "main" function_indexes); Hlt]
  with Not_found -> raise (Failure ("no \"main\" function"))
  in
    
  (* Compile the functions *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  { num_globals = List.length globals;
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function
	Jsr i when i > 0 -> Jsr func_offset.(i)
      | _ as s -> s) (List.concat func_bodies))
  }
*)
