(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f)
	  x)))))

(add-1 zero)

(add-1 (lambda (f) (lambda (x) x)))

(lambda (f) (lambda (x) (f ((lambda (g) (lambda (y) y)) f)


