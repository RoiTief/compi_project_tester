
let opt_tests : cg_test list = [
  {test = "(define f (lambda s 1))"; expected = ""};
  {test = "(define f (lambda (a . s) 1))"; expected = ""};
  {test = "((lambda s 1))"; expected = "1"};
  {test = "((lambda (a . s) 2) 1)"; expected = "2"};
  {test = "((lambda s 1) 1 2 3)"; expected = "1"};
  {test = "((lambda (a . s) 2) 1 2 3)"; expected = "2"};
  {test = "((lambda s s) 1 2 3)"; expected = "(1 2 3)"};
  {test = "((lambda (a . s) s) 1 2 3)"; expected = "(2 3)"};
  {test = "(define f (lambda (a b . s) s)) (f 1 2 3)"; expected = "(3)"};
  {test = "(let ((f (lambda s `(a new list ,s))) (a 1) (b 2)) (f a b))"; expected = "(a new list (1 2))"};
(*  
  {test = "
(letrec (
         (f (lambda (a . s) (g s)))
         (g (lambda s (if (__bin_equal_qq s '()) '(ended) (f s))))
        )
        (f 1 2 3 4))"; expected = "(ended)"};
   broken test *)
]
