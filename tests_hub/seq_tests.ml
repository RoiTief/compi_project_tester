
let seq_tests : cg_test list = [
  {test = "(begin 1 2 3)"; expected = "3"};
  {test = "(begin 'a 'b 'c)"; expected = "c"};
  {test = "(begin '())"; expected = "()"};
  {test = "(begin)"; expected = ""};
  {test = "(begin 1) (begin '()) (begin 2) (begin '()) (begin 3)"; expected = "1 () 2 () 3"};
  {test = "(begin 1 2 '(1 2 3))"; expected = "(1 2 3)"};
  {test = "(define x (begin 1 2 3)) 
x"; expected = "3"};
]
