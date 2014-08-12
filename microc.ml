open Printf

type action = Ast | (*Interpret | Bytecode |*) Compile

exception LexErr of string

let _ =
    
    let action = Printf.printf "here1";if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			     (* ("-i", Interpret);
			      ("-b", Bytecode);*)
			      ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = 
          Parser.comp Scanner.token lexbuf in
  match action with
    Ast -> let listing = Ast.string_of_comp program
           in print_string listing
  (*| Interpret -> ignore (Interpret.run program)
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing*)
  | Compile -> Compile.compile program (*Execute.execute_prog (Compile.translate program)*)
 
