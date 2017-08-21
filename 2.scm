(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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

