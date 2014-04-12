
(define (cube x) (* x x x))

(define (inc x) (+ x 1))

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
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)

(integral cube 0 1 0.001)

(integral cube 0 1 0.0001)


; h/3 * 


; go to n
; h = (b-a)/n
; at each k of n do f(a + kh)
; 0y0 + 4y1 + 2y2 + 4y3+y4 if n = 4 (n is always even)

(define (simpsons f a b n k)
  (define h (/ (- b a) n))
  (if (> k n)
      0
      (+ (* (coeff k n) (f (+ a (* k h))))
	 (simpsons f a b n (+ 1 k)))))

; Defines the coefficient used in the simpson's rule
(define (coeff k n)
  (cond ((= k 0) 1)
	((= k n) 1)
	((= (modulo k 2) 1) 4)
	((= (modulo k 2) 0) 2)))

(define (s f a b n)
  (define h (/ (- b a) n))
  (/ (* h (simpsons f a b n 0)) 3))

(s cube 0 1 1000.0) 



