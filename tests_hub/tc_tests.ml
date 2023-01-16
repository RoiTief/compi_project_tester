let tc_tests : cg_test list = [
  {test = "(define f (lambda (x) (__bin-add-qq x 2))) (f 1)"; expected = "3"};
  {test = "(letrec ((f (lambda (x) (g (__bin-add-qq x 1)))) (g (lambda (x) (__bin-add-qq x 2)))) (f 0))"; expected = "3"};
  {test = "((lambda (x) (eq? x 3)) 3)"; expected = "#t"};
]
