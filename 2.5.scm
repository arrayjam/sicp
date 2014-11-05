; 2.5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (rem number factor count)
  (if (not (= 0 (remainder number factor)))
    count
    (rem (/ number factor) factor (+ 1 count))))

(define (car x)
  (rem x 2 0))

(define (cdr x)
  (rem x 3 0))
