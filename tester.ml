#use "code-gen.ml";;
#load "str.cma";;

type cg_test = {test: string; expected: string};;

#use "tests_hub/const_tests.ml";;
#use "tests_hub/elias_tests.ml";;
#use "tests_hub/mayer_tests.ml";;
#use "tests_hub/seq_tests.ml";;
#use "tests_hub/if_tests.ml";;
#use "tests_hub/or_tests.ml";;
#use "tests_hub/tc_tests.ml";;

exception X_failed_test of string * string * string;; (* test, expected, actual *)

let closure_regex = Str.regexp "^#<closure";;
let sexprs output= (PC.star Reader.nt_sexpr output 0).found;;

let cg_tester test expected = 
  try
    let _ = Code_Generation.compile_scheme_string "foo.asm" test in
    let _ = Sys.command "make -s foo" in
    let run_in_ch = Unix.open_process_in "./foo | sed /^'!!!'/d | tr '\\n' ' '" in
    let actual = input_line run_in_ch in
    close_in run_in_ch;
    if expected = "closure" then
      if Str.string_match closure_regex actual 0 then
        ()
      else
        raise (X_failed_test(test, expected, actual))
    else 
      let expected_sexprs = sexprs expected
      and actual_sexprs = sexprs actual in
      if actual_sexprs = expected_sexprs then
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
    Printf.printf "\nFAILED %s tests\nTest string:\n%s\nExpected: %s\nActual: %s\n" kind test expected actual;
    exit 1;;

run_cg_tests const_tests "const";; (* testing constants *) 
run_cg_tests seq_tests "sequence";; (* testing sequencess *) 
run_cg_tests if_tests "'if' and 'and'";; (* testing if and 'and' *) 
run_cg_tests or_tests "or";; (* testing or *) 
run_cg_tests tc_tests "tail-call";; 
run_cg_tests elias_tests "Elias's";; (* all tests from Elias's tester *)
run_cg_tests mayer_tests "Mayer's";; (* Mayer's torture tests. These are not debuggable but give a good feeling that the compiler works. *)

