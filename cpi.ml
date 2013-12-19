type action = Ast | Interpret | Bytecode | Compile


let usage_msg = 
    "Cπ - Simplified C compiler for ARM V6\n" ^
    "cpi FILE [-o OUTFILE]\n" ^
    "-b print out bytecode \n" ^
    "-sast print out sast for program \n"

(* Default argument values *)
let out_file = ref "out"
let use_stdin = ref false
let use_stdout = ref false
let create_binary = ref false
let debug_bytecode = ref false
let debug_sast = ref false

(* Command line args *)
let speclist = 
    [
        ("--stdin", Arg.Set use_stdin, "Read from stdin" );
        ("--stdout", Arg.Set use_stdout, "Output to stdout" );
        ("-b", Arg.Set debug_bytecode, "Print out bytecode" );
        ("-sast", Arg.Set debug_sast, "Print out sast" );
        ("--binary", Arg.Set create_binary, 
        "Create binary executable (only if -o is set)" );
        ("-o", Arg.String (fun x -> out_file := x), "Set output file");
    ]


let save filename s =
    let channel = open_out filename in
    output_string channel s;
    close_out channel


(* Create and save executable binary file from assembly file *)
let create_binary_file filename =
    let filename_asm = filename ^ ".s" in
    let filename_obj = filename ^ ".o" in
    Sys.command ("as -o " ^ filename_obj ^ " " ^ filename_asm);
    Sys.command ("gcc -o " ^ filename ^ " " ^ filename_obj);
    (* Now clean up *)
    Sys.command ("rm -f " ^ filename_asm);
    Sys.command ("rm -f " ^ filename_obj);
    ()


let sast in_channel = 
    let lexbuf = Lexing.from_channel in_channel in
    let ast = Parser.program Scanner.token lexbuf in
    Typecheck.type_check_prog ast  


let program in_channel =
    let lexbuf = Lexing.from_channel in_channel in
    let ast = Parser.program Scanner.token lexbuf in
    Typecheck.type_check_prog ast;(*comment this line to disable type
    * checking*)
    Compile.translate ast 


(* Compiles from an input channel (stdin or source file) *)
(* If --stdout flag set, then print to stdout. else, save to out_file *)
let compile in_channel out_file =
    let asm = (Execute.execute_prog (program in_channel) ) in
        if !use_stdout then print_string asm
        else 
            save (out_file ^ ".s") asm;
            if !create_binary then create_binary_file out_file

let print_bytecode in_channel out_file = 
    let bytecode = Debug.dbg_str_program (program in_channel)  in
        if !use_stdout then print_string bytecode 
        else save (out_file ^ ".bytecode") bytecode

let print_sast in_channel out_file = 
    let sast_str = Debug.dbg_str_sast (sast in_channel)  in
        if !use_stdout then print_string sast_str
        else save (out_file ^ ".sast") sast_str

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

        if !use_stdin && !debug_bytecode then (print_bytecode stdin !out_file)
        else if !debug_bytecode then
            List.iter (fun f -> print_bytecode (open_in f) !out_file ) !source_files;

        if !use_stdin && !debug_sast then (print_sast stdin !out_file)
        else if !debug_sast then
            List.iter (fun f -> print_sast (open_in f) !out_file ) !source_files;
