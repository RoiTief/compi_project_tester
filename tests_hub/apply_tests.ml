
let apply_tests : cg_test list = [
  {test = "(__bin-apply __bin-add-qq '(1 2))"; expected = "3"};
  {test = "(define id (lambda (x) x)) (__bin-add-qq (__bin-apply (__bin-apply id `(,__bin-add-qq)) '(1 2)) 1)"; expected = "4"};
]
