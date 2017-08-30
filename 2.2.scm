;(define (cons x y)
;  (lambda (m) x y))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (iter z a)             ; the letter a was arbitrarily chosen here to match cons
    (if (even? z)
	(iter (/ z 2) (+ a 1))
	a))
  (iter z 0))

(define (cdr z)
  (define (iter z b)
    (if (= (remainder z 3) 0)
	(iter (/ z 3) (+ b 1))
	b))
  (iter z 0))

(define zero (lambda (f) (lambda (x) x)))    ; not the typical numeral zero, but a numeral that says
                                             ; 'apply f zero times'

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
        
(define zero-two-argument (lambda (f) (lambda (x) x) (lambda (x) x)))

(define (inc n)
  (+ 1 n))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))
