
let const_tests : cg_test list = [
  {test = "#t"; expected = "#t"};
  {test = "#f"; expected = "#f"};
  {test = "1"; expected = "1"};
  {test = "2"; expected = "2"};
  {test = "'()"; expected = "()"};
  {test = "(begin)"; expected = ""};
  {test = "\"asdf\""; expected = "\"asdf\""};
  {test = "#\\t"; expected = "#\\t"};
  {test = "#\\x"; expected = "#\\x"};
  {test = "'(a b c)"; expected = "(a b c)"};
  {test = "'(a b . c)"; expected = "(a b . c)"};
  {test = "'(a b . (c))"; expected = "(a b c)"};
  {test = "''(a . ((b) . (c)))"; expected = "(quote (a (b) c))"};
]
