#use "code-gen.ml";;
#use "tests_hub/const_tests.ml";;

exception X_failed_test of string * string * string;; (* test, expected, actual *)

let cg_tester test expected = 
  try
    let _ = Code_Generation.compile_scheme_string "foo.asm" test in
    let _ = Sys.command "make -s foo" in
    let run_in_ch = Unix.open_process_in "./foo | head -1" in
    let actual = input_line run_in_ch in
    close_in run_in_ch;
    if actual = expected then
      ()
    else
      raise (X_failed_test(test, expected, actual))
  with
  | X_syntax(syntax_err) -> raise (X_failed_test(test, expected, (Printf.sprintf "X_syntax(%s)" syntax_err)))
  | X_not_yet_implemented -> raise (X_failed_test(test, expected, "X_not_yet_implemented"))
  | X_this_should_not_happen(happened) -> raise (X_failed_test(test, expected, (Printf.sprintf "X_this_should_not_happen(%s)" happened)));;

let run_cg_tests (cg_tests : cg_test list) kind=
  try 
    Printf.printf "Starting %s tests\n" kind; 
    flush stdout;
    List.iter (fun (t : cg_test) -> cg_tester t.test t.expected; flush stdout) cg_tests ;
    Printf.printf "SUCCESSFULLY passed all %s tests for code-gen\n" kind;
    flush stdout
  with
  | X_failed_test(test, expected, actual) -> 
    Printf.printf "\nFAILED %s tests\nTest string: %s\nExpected: %s\nActual: %s\n" kind test expected actual;
    exit 1;;


run_cg_tests const_tests "const";; (* testing constants *)

