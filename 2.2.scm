;(define (cons x y)
;  (lambda (m) x y))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))
;warning: this function overrides cons
;(define (cons a b)
; (* (expt 2 a) (expt 3 b)))

;(define (car z)
;  (define (iter z a)             ; the letter a was arbitrarily chosen here to match cons
;    (if (even? z)
;	(iter (/ z 2) (+ a 1))
;	a))
;  (iter z 0))

;(define (cdr z)
;  (define (iter z b)
;    (if (= (remainder z 3) 0)
;	(iter (/ z 3) (+ b 1))
;	b))
;  (iter z 0))

(define zero (lambda (f) (lambda (x) x)))    ; not the typical numeral zero, but a numeral that says
                                             ; 'apply f zero times'

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
        
(define zero-two-argument (lambda (f) (lambda (x) x) (lambda (x) x)))

(define (inc n)
  (+ 1 n))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (positive? x)
  (>= x 0))

(define (negative? x)
  (< x 0))

(define (mul-interval x y)
        ;(+,+)*(+,+)  
  (cond ((and (positive? (lower-bound x)) (positive? (upper-bound x))
	      (positive? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	;(-,+)*(+,+)
	((and (negative? (lower-bound x)) (positive? (upper-bound x))
	      (positive? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
	;(-,-)*(+,+)
	((and (negative? (lower-bound x)) (negative? (upper-bound x))
	      (positive? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
	;(+,+)*(-,+)
	((and (positive? (lower-bound x)) (positive? (upper-bound x))
	      (negative? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	;(-,+)*(-,+) requires a call to max and min
	((and (negative? (lower-bound x)) (positive? (upper-bound x))
	      (negative? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
			(max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))
	;(-,-)*(-,+)
	((and (negative? (lower-bound x)) (negative? (upper-bound x))
	      (negative? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	;(+,+)*(-,-)
	((and (positive? (lower-bound x)) (positive? (upper-bound x))
	      (negative? (lower-bound y)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
	;(-,+)*(-,-)
	((and (negative? (lower-bound x)) (positive? (upper-bound x))
	      (negative? (lower-bound y)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
	;(-,-)*(-,-)
	((and (negative? (lower-bound x)) (negative? (upper-bound x))
	      (negative? (lower-bound y)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))
;going to need to redefine div-interval to use the percentage version of make-interval
(define (div-interval x y)
  (if (not (= (upper-bound y) 0))
      (if (not (= (lower-bound y) 0))
	  (mul-interval x
			(make-interval (/ 1.0 (upper-bound y))
				       (/ 1.0 (lower-bound y))))
	  (display "err: div by zero"))
      (display "err: div by zero")))

(define (make-interval a b) (cons a b))

(define (make-interval c p)
  (cons (- c (* c p)) (+ c (* c p))))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (lower-bound x) (lower-bound y))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x) ) 2.0) )

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i) ) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent x)
  (/ (width x) (center x)))

(define (product-percent x y)
  (/ (+ (percent y) (percent x)) (+ 1 (* (percent x) (percent y)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
