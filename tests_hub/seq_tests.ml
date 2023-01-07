
let seq_tests : cg_test list = [
  {test = "(begin 1 2 3)"; expected = "3"};
  {test = "(begin 'a 'b 'c)"; expected = "c"};
  {test = "(begin '())"; expected = "()"};
  {test = "(begin)"; expected = ""};
  (*
  {test = "(begin (display-sexpr (begin 1)) (write-char #\\newline)) (begin (display-sexpr (begin 2)) (write-char #\\newline)) (begin (display-sexpr (begin 3)) (write-char #\\newline))"; expected = "1
2
3"};
     saved for later *)
  {test = "(begin 1 2 '(1 2 3))"; expected = "(1 2 3)"};
  {test = "(define x (begin 1 2 3)) 
x"; expected = "3"};
]
