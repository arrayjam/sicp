; 2.1
(define (make-rat n d)
  (define normalise-sign
    (cond ((and (< n 0) (< d 0)) 1)
	  ((or (< n 0) (< d 0)) -1)
	  (else 1)))
  (let ((g (gcd n d))
	(sign normalise-sign)
    (cons (* sign (abs (/ n g))) (abs (/ d g))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))


(define neg-one-half (make-rat 1 -2))

(print-rat neg-one-half)

(print-rat (mul-rat neg-one-half neg-one-half))

(print-rat (div-rat (make-rat -1 -5) (make-rat -53 3)))


; 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cons z)
  (z (lambda (p q) q)))
