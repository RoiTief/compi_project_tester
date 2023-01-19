let box_tests : cg_test list = [
  {test = "(procedure? ((let () (letrec((foo (lambda() foo))) foo ))))"; expected = "#t"};
]
