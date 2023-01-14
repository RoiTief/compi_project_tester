
let set_tests : cg_test list = [
  {test = "(define x 1) x (set! x '()) x"; expected = "1 ()"};
  {test = "(define x 1) (define y x) y (set! x '()) x y"; expected = "1 () 1"};
  {test = "(define x '(a b c)) (define y `(1 2 ,x)) (set! x 3) y"; expected = "(1 2 (a b c))"};
  {test = "(define x '(a b c)) (define y `(1 2 ,@x)) (set! x 3) y"; expected = "(1 2 a b c)"};
  {test = "(define foo3
				(lambda (x y)
				(lambda () x)
				(lambda () y)
				(lambda () (set! x y) x)))
				
			((foo3 5 10)) "; expected = "10"};
      
  {test = "(define foo4
				(lambda (x y)
				(if x
				(lambda () (set! y x) y)
				(lambda (z) (set! x z) x))))
				
		    ((foo4 #f 6) 8)"; expected = "8"};
        
  {test = "(define foo4
				(lambda (x y)
				(if x
				(lambda () (set! y x) y)
				(lambda (z) (set! x z) x))))
				((foo4 5 6)) "; expected = "5"};
        
   {test = "(define foo8
				(lambda (x y)
					(cons x
						(lambda ()
						(set! x y) x))))
				(car (foo8 8 9))"; expected = "8"};
]
