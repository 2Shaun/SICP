(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (average a b)
  (/ (+ a b) 2))

(define (make-rat n d)
  (cond ((or (and (> n 0) (> d 0)) (and (< n 0) (< d 0)) (and (> n 0) (< d 0)))
	(let ((g (- (gcd n d))))
	  (cons (/ n g) (/ d g))))
	(else
	 (let ((g (- (gcd n d))))
	  (cons (/ n g) (/ d g))))))

(define (make-rat n d)
  (cond ((or (and (< n 0) (> d 0)) (and (> n 0) (< d 0)))
	 (let ((g (- (gcd n d))))    ; swaps signs on pair
	   (cons (/ n g) (/ d g))))
	(else
	 (let ((g (gcd n d)))
	   (cons (/ n g) (/ d g))))))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-point x y) (cons x y))

(define (p1 s) (car s))

(define (p2 s) (cdr s))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (midpoint seg)
  (make-point (average (x-point (p1 seg)) (x-point (p2 seg)))
	      (average (y-point (p1 seg)) (y-point (p2 seg)))))

(print-point (midpoint (make-segment (make-point 1 2) (make-point 2 3))))

(define (distance seg)
  (sqrt (+ (square (- (x-point (p1 seg)) (x-point (p2 seg))))
	   (square (- (y-point (p1 seg)) (y-point (p2 seg)))))))

(define (rectangle seg1 seg2)  ; doesn't validate that seg1 is touching seg2 (they must share a point
  (cons seg1 seg2))

(define (length rect)
  (distance (car rect)))

(define (width rect)
  (distance (cdr rect)))

(define (area rect)
  (* (length rect) (width rect)))

(define (perimeter rect)
  (+ (* 2 (length rect)) (* 2 (width rect))))

(define l (make-segment (make-point 1 1) (make-point 1 3)))

(define w (make-segment (make-point 1 3) (make-point 4 3)))

(define r (rectangle l w))

(area r)
