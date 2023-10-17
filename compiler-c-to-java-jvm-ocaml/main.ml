let argc = Array.length Sys.argv in
    if argc != 2 then
   	begin
        	prerr_endline("Sintassi: ./compilatore <nome file .c>\n");
       		exit 1
    	end;
;;
let file_quanto = String.length Sys.argv.(1)
let file_estensione = String.sub Sys.argv.(1) (file_quanto - 2) 2
let file_nome = String.sub Sys.argv.(1) 0 (file_quanto -2) 
;; 
if (String.compare file_estensione ".c") != 0 then
  	begin
        	prerr_endline("ATTENZIONE: il file specificato non ha estensione .c\n");
        	exit 1
   	end;
;;
if not(Sys.file_exists (file_nome ^ ".c")) then
	begin
	        prerr_endline("ATTENZIONE: il file specificato non esiste\n");
        	exit 1
	end;
;;

let dfile = open_in (file_nome ^ ".c")
let lexbuf = Lexing.from_channel dfile 
let outfile = open_out (file_nome ^ ".j")

let cls = J.Class.create (J.Path.parse file_nome)  

let _=
try

	let rec aux delta = 
		
		let ast = Parser.main Lexer.token lexbuf in
		let delta' = Type.wfs2 delta ast in  (* alla type passo delta*)
                let compiler = Compilatore.compila_gdecl file_nome delta' cls ast (*ripasso ast
                a compilatore*)in 
		aux delta'	
	in
		aux [](*,
                                J.Class.serialize outfile Compilatore.classe_jvm *)

  
with
	Lexer.Eof -> 
  
(*outfile*)
J.Class.serialize outfile cls;;
prerr_endline("\nCOMPILAZIONE...");;
prerr_endline("GENERAZIONE DEL FILE CLASS...");;
close_out (outfile);;
Sys.command ("java -jar jasmin.jar " ^ file_nome ^ ".j");;
prerr_endline("COMPLETATO\n");;
