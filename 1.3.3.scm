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
  (define (cont-frac i)
    (if (= i k)
	0
	(/ (n i) (+ (d i) (cont-frac n d k (+ 1 i))))))
  (cont-frac 0))

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

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(sqrt 2)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(sqrt 2)

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

(sqrt 2)

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (cr a b c x)
  (newtons-method (cubic a b c) 1))

(cr 5 2 1 0)

; 1.41
(define (double f)
  (lambda (x) (f (f x))))  

(define (inc x) (+ 1 x))

((double inc) 0)

(((double (double double)) inc) 0)

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3)))

(define (n-smooth f n)
  (repeated (smooth f) n))

(define (pow x n)
  (cond ((< n 0) (error "Squares, not roots"))
	((= n 0) 1)
	((= n 1) x)
	(else (* x (pow x (- n 1))))))

(pow 3 0)

; 1.45
(define (nth-root n x)
  (fixed-point ((repeated average-damp (- n 2)) (lambda (y) (/ x (pow y (- n 2))))) 1.0))

(nth-root 3 2) ; 1.148698

; TODO 1.46
(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (iterate current-guess)
      (let ((new-guess (improve initial-guess current-guess)))
	(if (good-enough? current-guess new-guess)
	    new-guess
	    (iterate new-guess))))
    (iterate initial-guess)))

(define (sqrt x)
  ((iterative-improve (lambda (guess x) (< (abs (- (square guess) x)) dx))
		      (lambda (guess x) (average guess (/ x guess)))) 1.0))

(sqrt 2)
