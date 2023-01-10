
let set_tests : cg_test list = [
  {test = "(define x 1) x (set! x '()) x"; expected = "1 ()"};
  {test = "(define x 1) (define y x) y (set! x '()) x y"; expected = "1 () 1"};
  {test = "(define x '(a b c)) (define y `(1 2 ,x)) (set! x 3) y"; expected = "(1 2 (a b c))"};
  {test = "(define x '(a b c)) (define y `(1 2 ,@x)) (set! x 3) y"; expected = "(1 2 a b c)"};
]
