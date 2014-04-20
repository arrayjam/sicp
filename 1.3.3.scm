(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
		      1.0
		      2.0)


(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
	     1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

(sqrt 2)

; 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     2.0)

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
	     2.0)

; 1.37
(define (cont-frac-rec n d k)
  (define (cont-frac n d k i)
    (if (= i k)
	0
	(/ (n i) (+ (d i) (cont-frac n d k (+ 1 i))))))
  (cont-frac n d k 0))

(define (cont-frac-iter n d k)
  (define (cont-frac n d k result)
    (if (= k -1)
	result
	(cont-frac n d (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac n d (- k 1) 0.0))


; 1/phi = 0.61803398875
(cont-frac-rec (lambda (i) 1.0)
	       (lambda (i) 1.0)
	       10)

(cont-frac-iter (lambda (i) 1.0)
		(lambda (i) 1.0)
		10)

; 1.38
(define (d i)
  (if (not (= (modulo (+ i 2) 3) 0))
      1
      (* 2 (/ (+ i 2) 3.0))))

 (cont-frac-rec (lambda (x) 1.0)
 	       d
 	       1000)

 (cont-frac-iter (lambda (x) 1.0)
 		d
 		1000)

(define (tan-cf x k mode)
  (/ x (- 1 (mode (lambda (a) (* a a))
			    (lambda (a) 2.0)
			    k))))

(tan-cf 3.4 1000 cont-frac-rec)

(tan-cf 3.4 1000 cont-frac-iter)
