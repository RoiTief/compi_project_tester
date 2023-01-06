#use "compiler.ml"

let make_make_label_tests prefix =
  let index = ref 0 in
  fun () ->
  (index := !index + 1;
    Printf.sprintf "%s_%03d" prefix !index);;

let make_file_name = 
  let helper = (make_make_label_tests "./foo") in
  fun () -> helper();;

type cg_test = {test: string; expected: string};;

let cg_tests : cg_test list = [
  {test = "#t"; expected = "#t"};
  {test = "1"; expected = "1"};
  {test = "(begin 1 #t)"; expected = "#t"};
  {test = "(lambda (a b) a)"; expected = "closure"};
  {test = "((lambda (a b) a) 1 2)"; expected = "1"};
  {test = "((lambda (a b) b) 3)"; expected = "too few arguements"};
  {test = "((lambda (a b) b) 3 2 5)"; expected = "too many arguements"};
  {test = "(lambda (x) (set! x 1) (lambda () x))"; expected = "closure"};
  {test = "(define a (lambda (n) (begin (lambda () (set! n (+ n 1)) n) (lambda () (set! n 0)))))"; expected = " "};
  {test = "(lambda a a)"; expected = "closure"};
  {test = "((lambda a a))"; expected = "()"};
  {test = "((lambda (a . b) b) )"; expected = "too few arguments"};
  {test = "((lambda (a . b) b) 1)"; expected = "()"};
  {test = "((lambda (a . b) b) 1 2)"; expected = "(2)"};
  {test = "((lambda (a . b) b) 10 20 30 40)"; expected = "(20 30 40)"};
  {test = "((lambda (a) (a)) (lambda () 1))"; expected = "1"};
  {test = "(define list (lambda args args)) (let ((z 78) (w 89) (x 2) ) (list z w x 3)))"; expected = "(78 98 2 3)"};

  {test = "(letrec ((run (lambda (a . s) s)) ) (run 1 2 3 4 5)))"; expected = "(2 3 4 5)"};
  {test = "((letrec ((run (lambda (a . s) s)) ) (lambda (b . c) (run b c))) 1 2 3)"; expected = "((2 3))"};
  {test = "(let ((lst (lambda args args))) (lst 1))"; expected = "(1)"}; (*you can add '(define list (lambda args args))' instead of using init file*)
  {test = "(((lambda (a . args) (lambda () args)) 2 3 4))"; expected = "(3 4)"};

  {test = "(((((lambda (a) (lambda (b) (((lambda (a) (lambda (b) ((a b) (lambda
  (x) (lambda (y) y))))) ((lambda (n) ((n (lambda (x) (lambda (x)
  (lambda (y) y)))) (lambda (x) (lambda (y) x)))) (((lambda (a) (lambda
  (b) ((b (lambda (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n
  (lambda (p) (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
  ((lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p
  (lambda (a) (lambda (b) a)))) p))) ((lambda (p) (p (lambda (a) (lambda
  (b) a)))) p)))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
  (lambda (x) (lambda (y) y))) (lambda (x) (lambda (y) y))))))) a))) a)
  b))) ((lambda (n) ((n (lambda (x) (lambda (x) (lambda (y) y))))
  (lambda (x) (lambda (y) x)))) (((lambda (a) (lambda (b) ((b (lambda
  (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n (lambda (p)
  (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) ((lambda (n)
  (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p (lambda (a)
  (lambda (b) a)))) p))) ((lambda (p) (p (lambda (a) (lambda (b) a))))
  p)))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) (lambda (x)
  (lambda (y) y))) (lambda (x) (lambda (y) y))))))) a))) b) a)))))
  ((lambda (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n (lambda
  (p) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) ((lambda (n)
  (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p (lambda (a)
  (lambda (b) a)))) p))) (((lambda (a) (lambda (b) ((b (a (lambda (a)
  (lambda (b) ((a (lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))))
  b))))) (lambda (x) (lambda (y) y))))) ((lambda (p) (p (lambda (a)
  (lambda (b) a)))) p)) ((lambda (p) (p (lambda (a) (lambda (b) b))))
  p))))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) (lambda (x)
  x)) (lambda (x) x))))) (lambda (x) (lambda (y) (x (x (x (x (x
  y))))))))) (((lambda (a) (lambda (b) ((b (a (lambda (a) (lambda (b)
  ((a (lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))) b)))))
  (lambda (x) (lambda (y) y))))) (((lambda (a) (lambda (b) ((b (a
  (lambda (a) (lambda (b) ((a (lambda (n) (lambda (s) (lambda (z) (s ((n
  s) z)))))) b))))) (lambda (x) (lambda (y) y))))) ((lambda (x) (lambda
  (y) (x (x (x y))))) (lambda (x) (lambda (y) (x (x y)))))) (lambda (x)
  (lambda (y) (x (x (x y))))))) (lambda (x) (lambda (y) (x (x (x (x (x
  y))))))))) #t) #f)"; expected = "#t"};

    {test = 
    "(let ()
      ((lambda s
        (let ()
          ((lambda s s) s s s)))
      #t))"; expected = "((#t) (#t) (#t))"};

    {test = 
    "(let ((list (lambda args args))) 
      ((lambda (a . s)
      (list a s
      ((lambda (a b . s)
      ((lambda (a b c . s)
      (list a b c s
      ((lambda (a b c d . s)
      (list a b c d s))
      1001 1002 1003 1004 1005)))
      101 102 103 104 105))
      11 12 13 14 15)))
      1 2 3 4 5))"; expected = "(1 (2 3 4 5) (101 102 103 (104 105) (1001 1002 1003 1004 (1005))))"};


    {test = "((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x ((lambda x x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"; expected = "()"};

    {test = "(let ((e '(a b c d e f g)))
    (cons (cdr e) (cons (car e) (cons 'moshe '()))))"; expected = "((b c d e f g) a moshe)"};
    {test = "(let ((e '(a))) (cons (car e) (car e)))"; expected = "(a . a)"};
    {test = "(define e '(a)) (cons (car e) (car e)) "; expected = "(a . a)"};
    {test = "(cons 'a 'a)"; expected = "(a . a)"};
    {test = "(car '(a))"; expected = "a"};
    {test = "(cons (car '(a)) 'moshe)"; expected = "(a . moshe)"};
    {test = "(cons 'moshe (car '(a))) "; expected = "(moshe . a)"};
    {test = " (define list (lambda args args))
    ((lambda (a . s)
    (list a s)) 1 2 3 4 5 ))"; expected = "(1 (2 3 4 5))"};
    {test = 
    " (define list (lambda args args)) 
    ((lambda (a . s)
    (list a s
    ((lambda (a b . s)
    ((lambda (a b c . s)
    (list a b c s
    ((lambda (a b c d . s)
    (list a b c d s))
    1001 1002 1003 1004 1005)))
    101 102 103 104 105))
    11 12 13 14 15)))
    1 2 3 4 5)"; expected = "(1 (2 3 4 5) (101 102 103 (104 105) (1001 1002 1003 1004 (1005))))"};
    {test = "(define list (lambda args args))
    ((lambda (a b c . s)
    (list a b c s
        ((lambda (a b c d . s)
          (list a b c d s))
        1001 1002 1003 1004 1005)))
    101 102 103 104 105)"; expected = "(101 102 103 (104 105) (1001 1002 1003 1004 (1005)))"};
    {test = "(__bin-apply cons '(#t #f))";
     expected = "(#t . #f)"};
    {test = "(__bin-apply __bin-apply (cons __bin-apply (cons (cons cons (cons (cons #t (cons #f '())) '())) '())))";
     expected = "(#t . #f)"};
    {test = "(__bin-apply __bin-apply (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons __bin-apply (cons (cons cons (cons (cons #t (cons #f '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())) '())))";
     expected = "(#t . #f)"};
    {test = "(apply + '(1 2 3))";
     expected = "6"};
    {test = "(apply + '(1 2 3))"; expected = "6"};
    {test = "(apply + '(1))"; expected = "1"};
    {test = "(apply + 0 '(8 5 4 3))"; expected = "20"};
    {test = " (apply + '(1))"; expected = "1"};
    {test = "(apply + 1 '(8 5 4 3))"; expected = "21"};
    {test = " (apply + 7 8 '(8 5 4 3))"; expected = "35"};
    {test = "(apply append '(3) '((5)))"; expected = "(3 5)"};
    {test = " (apply cons #t '(7))"; expected = "(#t . 7)"};
    {test = "(apply boolean? '(#t))"; expected = "#t"};
    {test = "(apply number? '(5))"; expected = "#t"};
    {test = "(apply eq? 5 '(5))"; expected = "#t"};
    {test = "(apply list 8 '(5 7 4))"; expected = "(8 5 7 4)"};
    {test = "(apply make-string 5 '(#\\a) )"; expected = "\"aaaaa\""};
    {test = "(apply make-vector 3 '(#\\b))"; expected = "#(#\\b #\\b #\\b)"};
    {test = "(apply (lambda (x y t) (+ x y t)) '(1 2 32/33) )"; expected = "131/33"};
    {test = "(apply (lambda (x y t) (< x y t)) '(1 2 32/33) )"; expected = "#f"};
    {test = "(apply (lambda (x y t) (> x y t)) '(1 2 32/33))"; expected = "#f"};
    {test = "(apply (lambda (x y t) (- x y t)) '(1 2 32/33) )"; expected = "-65/33"};
    {test = "(apply (lambda (x y t) (= x y t)) '(1 2 32/33))"; expected = "#f"};
    {test = "(apply (lambda (x y t) (* x y t)) '(1 2 32/33))"; expected = "64/33"};
    {test = "(apply (lambda (x y t) (/ x y t)) '(1 2 32/33))"; expected = "33/64"};
    {test = "(apply + '(1 2 3 4 5 32/33))"; expected = "527/33"};
];;

let cg_tester str expected filename = 
  let _ = Printf.printf "Expected:\n%s\n--------------------------------\n" expected in 
  try 
    let _ = Code_Generation.compile_scheme_string (filename ^ ".asm") str in
    let _ = Sys.command("make -s " ^ filename ^ ";" ^ filename) in
    ()
  with 
  | X_syntax(syntax_err) -> Printf.printf "\nTest String: %s, Result: X_syntax(%s)\n" str syntax_err
  | X_not_yet_implemented -> Printf.printf "\nTest String: %s, Result: X_not_yet_implemented\n" str
  | X_this_should_not_happen(happened) -> Printf.printf "\nTest String: %s, X_this_should_not_happen(%s)\n" str happened

let rec my_map f = function 
  | [] -> ()
  | first :: rest -> let filename = make_file_name() in
                      match Unix.fork() with 
                        | 0 -> f first filename; exit 0
                        | pid -> ignore(Unix.waitpid [] pid) ; my_map f rest;;
let run_cg_tests (cg_tests : cg_test list) =
  let _ = my_map (fun (t : cg_test) -> cg_tester t.test t.expected) cg_tests in
  let _ = Printf.printf "Finished Running tests for code-gen\n" in
    ();;

let cg_tests2 = [ (*run specific tests here, don't forget to change the input for 'run_cg_tests'*)
    (* {test = "((lambda (x) (x x)) (lambda (x) (x x)))"}; *) (*checks tpapplic memory leaks*)
    (* {test = "(letrec ((a (lambda (x y) (y y y x)))
              (b (lambda (x y z) (z z x))))
              (a a b)) "; expected = "?"}; *)
];;
run_cg_tests cg_tests;;


