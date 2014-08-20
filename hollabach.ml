open Printf

type action = Ast | (*Interpret | Bytecode |*) Compile

exception LexErr of string

let _ =
    
    let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			     (* ("-i", Interpret);
			      ("-b", Bytecode);*)
			      ("-c", Compile) ]
  else Compile in
    
    let outfile = Sys.argv.(2) in
    let delIfExists o =
                if Sys.file_exists o then Sys.remove(o) else ()
    in

  let lexbuf = Lexing.from_channel stdin in
  let program = 
          List.rev (Parser.program Scanner.token lexbuf) in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
  (*| Interpret -> ignore (Interpret.run program)
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing*)
  | Compile -> delIfExists outfile;Compile.compile program outfile; ()
  
 
