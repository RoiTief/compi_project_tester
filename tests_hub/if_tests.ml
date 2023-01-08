
let if_tests : cg_test list = [
  {test = "(if #t #t #f)"; expected = "#t"};
  {test = "(if #f #t #f)"; expected = "#f"};
  {test = "(if 1 2 3)"; expected = "2"};
  {test = "(if (if 1 2 3) 4 5)"; expected = "4"};
  {test = "(if (if #f 2 3) 4 5)"; expected = "4"};
  {test = "(and)"; expected = "#t"};
  {test = "(and 1 2 3)"; expected = "3"};
  {test = "(and 1 #f 3)"; expected = "#f"};
  {test = "(and (and 2))"; expected = "2"};
  {test = "(if (and) 1 2)"; expected = "1"};
  {test = "
(define x #\\x) 
(define y (begin (if 1 2 3) (and)))
(define z (and x))
(and (if z y x))"; expected = "#t"};
]

