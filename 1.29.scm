
(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter 0 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 2.3 0.01)

(integral cube 0 2.3 0.001)

(integral cube 0 2.3 0.0001)


; h/3 * 
		  
; go to n
; h = (b-a)/n
; at each k of n do f(a + kh)
; 0y0 + 4y1 + 2y2 + 4y3+y4 if n = 4 (n is always even)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (a-sum term a next b) (accumulate + 0 term a next b))
(define (a-product term a next b) (accumulate * 1 term a next b))


(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (term k) (* (coeff k n) (f (+ a (* k h)))))
  (/ (* h (a-sum term 0 inc n)) 3))

; Defines the coefficient used in the simpson's rule
(define (coeff k n)
  (cond ((= k 0) 1)
	((= k n) 1)
	((= (modulo k 2) 1) 4)
	((= (modulo k 2) 0) 2)))

(simpsons cube 0 2.3 6) 

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity x) x)

(define (wallis-pi n)
  (define (next-even x) (if (even? x) x (+ 1 x)))
  (define (next-odd x) (if (odd? x) x (+ 1 x)))
  (define (num-f x) (next-even (+ 2 x)))
  (define (den-f x) (next-odd (+ 2 x)))
  (* 4.0 (/ (a-product num-f 0 inc n) (a-product den-f 0 inc n))))

(wallis-pi 1)
(wallis-pi 2)
(wallis-pi 3)
(wallis-pi 4)
(wallis-pi 5)
(wallis-pi 1000)

