type action = Ast | Interpret | Bytecode | Compile


let usage_msg = 
    "Cπ - Simplified C compiler for ARM V6\n" ^
    "cpi FILE [-o OUTFILE]\n"

(* Default argument values *)
let out_file = ref "out.s"
let use_stdin = ref false
let use_stdout = ref false

(* Command line args *)
let speclist = 
    [
        ("--stdin", Arg.Set use_stdin, "Read from stdin" );
        ("--stdout", Arg.Set use_stdout, "Output to stdout" );
        ("-o", Arg.String (fun x -> out_file := x), "Set output file");
    ]


let save filename s =
     let channel = open_out filename in
     output_string channel s;
     close_out channel


(* Compiles from an input channel (stdin or source file) *)
(* If --stdout flag set, then print to stdout. else, save to out_file *)
let compile in_channel out_file =
    let program =
        let lexbuf = Lexing.from_channel in_channel in
        let prog = Parser.program Scanner.token lexbuf in
        Compile.translate prog in

    let asm = (Execute.execute_prog program) in
        if !use_stdout then print_string asm
        else save out_file asm
        

(* MAIN *)
let main = 
    (* Assume all anonymous arguments are source files and add them to
     * source_files list *)
    let source_files = ref [] in
        Arg.parse speclist (fun file -> source_files := file::!source_files ) usage_msg;

        (* If --stdin flag is set, read source from stdin *)
        (* Else, read from input source files *)
        if !use_stdin then (compile stdin !out_file) else
            List.iter (fun f -> compile (open_in f) !out_file ) !source_files;

