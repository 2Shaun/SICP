#lang racket

(define >=
  (lambda (x y)
    (not (< x y))
    )
  )

(define <=
  (lambda (x y)
    (not (> x y))
    )
  )

(define abs
  (lambda (x)
    (if (>= x 0)
	x
	(- x))
    )
  )

(define square
  (lambda (x) (* x x))
  )

(define sum-of-squares
  (lambda (x y)
    (+ (square x) (square y))
    )
  )

(define (square-sum-larger a b c)
  (if (>= a b)
      (if (>= b c)
	  (sum-of-squares a b)
	  (sum-of-squares a c))
      (if (>= a c)
	  (sum-of-squares b a)
	  (sum-of-squares b c))
      )
  )

(define cbrt
  (lambda (x)
    (cbrt-try x 1.0)
    )
  )

(define cbrt-try
  (lambda (x guess)
    (if (< (abs (- (cbrt-improve x guess) guess)) 0.0001)
	guess
	(cbrt-try x (cbrt-improve x guess))
	)
    )
  )

(define cbrt-improve
  (lambda (x guess)
    (/ (+ (/ x (square guess))
	  (* 2 guess)) 3)
    )
  )

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (f n)
  (define (f-iter sum counter)
    (if (> counter 3)
        sum
        (f-iter (+ (* counter (f (- n counter))) sum) (+ counter 1))
        )
    )
  (if (< n 3)
      n
      (f-iter 0 1)
      )
  )

(define (infinity)
  (define (iter counter)
    (iter (+ counter 1))
    )
  (iter 0)
  )

(infinity)


