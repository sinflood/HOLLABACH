open Printf

type action = Ast | Compile

exception LexErr of string

let _ =
    
    let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-c", Compile) ]
  else Compile in
    
    let outfile = Sys.argv.(2) in
   (*remove the output file if it exists as it is likely a past version*)
    let delIfExists o =
                if Sys.file_exists o then Sys.remove(o) else ()
    in

  let lexbuf = Lexing.from_channel stdin in
  let program = 
          List.rev (Parser.program Scanner.token lexbuf) in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
  | Compile -> delIfExists outfile;Compile.compile program outfile;()
  
 
