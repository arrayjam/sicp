; 2.2
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

(print-point (make-point 3 4))

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-segment segment)
  (print-point (start-segment segment))
  (display " - ")
  (print-point (end-segment segment))
  (newline))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
	      (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (distance-between-points x1 x2 y1 y2)
  (sqrt (+ (expt (- y2 y1) 2)
	   (expt (- x1 x2) 2))))

(define (coordinates-of-segment segment f)
  (let ((p1 (start-segment segment))
	(p2 (end-segment segment)))
    (coordinates-of-points p1 p2 f)))

(define (coordinates-of-points p1 p2 f)
    (let ((x1 (x-point p1))
	  (x2 (x-point p2))
	  (y1 (y-point p1))
	  (y2 (y-point p2)))
      (f x1 x2 y1 y2)))

(define (length-segment segment)
  (coordinates-of-segment segment distance-between-points))

(define seg (make-segment (make-point 0 3) (make-point 3 0)))

(print-segment seg)

(print-point (midpoint-segment seg))

(length-segment seg)

(define (make-rect p1 p2)
  (cons p1 p2))

(define (p1-rect rect)
  (car rect))

(define (p2-rect rect)
  (cdr rect))

(define (w-segment-rect rect)
  (coordinates-of-points
   (p1-rect rect) (p2-rect rect)
   (lambda (x1 x2 y1 y2)
     (make-segment (make-point x1 y1)
		   (make-point x2 y1)))))

(define (h-segment-rect rect)
  (coordinates-of-points
   (p1-rect rect) (p2-rect rect)
   (lambda (x1 x2 y1 y2)
     (make-segment (make-point x1 y1)
		   (make-point x1 y2)))))

(define (perimiter-rect rect)
  (+ (* (length-segment (w-segment-rect rect)) 2)
     (* (length-segment (h-segment-rect rect)) 2)))

(define (area-rect rect)
  (* (length-segment (w-segment-rect rect))
     (length-segment (h-segment-rect rect))))

(define rect (make-rect (make-point 0 0) (make-point 4 4)))
(print-segment (make-segment (make-point 0 0) (make-point 4 4)))
(print-segment (w-segment-rect rect))
(print-segment (h-segment-rect rect))

(perimiter-rect rect)
(area-rect rect)

(define (make-rect origin width height)
  (cons origin (cons width height)))

(define (origin-rect rect)
  (car rect))

(define (width-rect rect)
  (car (cdr rect)))

(define (height-rect rect)
  (cdr (cdr rect)))

(define (parameters rect f)
  (let ((origin (origin-rect rect))
	(width (width-rect rect))
	(height (height-rect rect)))
    (f origin width height)))

(define (h-segment-rect rect)
  (parameters
   rect
   (lambda (origin width height)
     (make-segment origin
		   (make-point (x-point origin)
			       (- (y-point origin) height))))))

(define (w-segment-rect rect)
  (parameters
   rect
   (lambda (origin width height)
     (make-segment origin
		   (make-point (+ (x-point origin) width)
			       (y-point origin))))))

(define origin (make-point 0 0))

(define rect (make-rect origin 4 4))
(origin-rect rect)
(width-rect rect)
(height-rect rect)
(h-segment-rect rect)
(w-segment-rect rect)

(perimiter-rect rect)

(area-rect rect)

;(define (area-rect rect)
;  (make-segment (p1-rect rect


