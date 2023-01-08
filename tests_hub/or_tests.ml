
let or_tests : cg_test list = [
  {test = "(or)"; expected = "#f"};
  {test = "(or 1 2 3)"; expected = "1"};
  {test = "(or 1 #f 3)"; expected = "1"};
  {test = "(or #f #f #f 3)"; expected = "3"};
  {test = "(or (and))"; expected = "#t"};
  {test = "(or (or))"; expected = "#f"};
  {test = "(if (or) 1 2)"; expected = "2"};
  {test = "(or (and (or (and (or (and (or (or))))))))"; expected = "#f"};
  {test = "(or (and (or (and (or (and (or (and))))))))"; expected = "#t"};
  {test = "(or (and (or (and (or (and (or 3)))))))"; expected = "3"};
]

