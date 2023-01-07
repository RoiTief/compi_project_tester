
let elias_failure_tests : cg_test list = [
  {test = "((lambda (a b) b) 3)"; expected = "too few arguements"};
  {test = "((lambda (a b) b) 3 2 5)"; expected = "too many arguements"};
  {test = "((lambda (a . b) b) )"; expected = "too few arguments"};
]

let elias_tests : cg_test list = [
  {test = "#t"; expected = "#t"};
  {test = "1"; expected = "1"};
  {test = "(begin 1 #t)"; expected = "#t"};
  {test = "(lambda (a b) a)"; expected = "closure"};
  {test = "((lambda (a b) a) 1 2)"; expected = "1"};
  {test = "(lambda (x) (set! x 1) (lambda () x))"; expected = "closure"};
  {test = "(define a (lambda (n) (begin (lambda () (set! n (+ n 1)) n) (lambda () (set! n 0)))))"; expected = ""};
  {test = "(lambda a a)"; expected = "closure"};
  {test = "((lambda a a))"; expected = "()"};
  {test = "((lambda (a . b) b) 1)"; expected = "()"};
  {test = "((lambda (a . b) b) 1 2)"; expected = "(2)"};
  {test = "((lambda (a . b) b) 10 20 30 40)"; expected = "(20 30 40)"};
  {test = "((lambda (a) (a)) (lambda () 1))"; expected = "1"};
  {test = "(define list (lambda args args)) (let ((z 78) (w 89) (x 2) ) (list z w x 3)))"; expected = "(78 89 2 3)"};

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
