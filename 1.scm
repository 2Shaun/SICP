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

(define power
  (lambda (x n)
    (define iter
      (lambda (count)
	(if (= count 0)
	    1
	    (* x (power x (- n 1))))))
    (iter n)))

(define phi
  (lambda (n)
    (power (/ (+ 1 (sqrt 5)) 2) n)))

(define psi
  (lambda (n)
    (power (/ (- 1 (sqrt 5)) 2) n)))

(define fib-formula
  (lambda (n)
    (/ (- (phi n) (psi n)) (sqrt 5))))

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
(define (f n)
  (define (iter sum counter)
    (if (> counter 3)
	sum
	(iter (+ (* counter (f (- n counter))) sum) (+ counter 1))
	)
    )
  (if (< n 3)
      n
      (iter 0 1)
      )
  )

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (* 1 (f-recursive (- n 1)))
	 (* 2 (f-recursive (- n 2)))
	 (* 3 (f-recursive (- n 3))))
      )
  )


(define (pascals-triangle row)
  (define (iter choose)
        (cond ((= choose row) (display 1)(display #\space))
              ((= choose 0) (display 1)(display #\space) (iter (+ choose 1)))
              (else (display (/ (factorial row)
                                (* (factorial choose)
                                   (factorial (- row choose)))))(display #\space)
                    (iter (+ choose 1)))))
    (cond ((= row 0) (display 1)(display #\space))
        (else (iter 0)(newline)(pascals-triangle (- row 1)))))

(define (infinity x)
  (display x)
  (infinity (+ 1 x)))

(define (limit n)
  (display (/ (+ (* 3 n) 1) n))
  (limit (+ n 1)))

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (expt-iter b n)
  (define (iter product counter)
    (if (= counter n)
	product
	(iter (* product b) (+ 1 counter))))
  (iter b 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (fast-expt-iter b n)
  (define (iter b counter product)
    (cond ((= counter 0) 1)
	  ((even? counter) (* product (iter b (half counter) (square b))))
	  (else (* product (iter b (- counter 1) (* product b))))))
  (iter b n 1))

(define (double n)
  (* 2 n))

(define (half n)
  (/ n 2))

(define (fast-multiply b n)
  (cond ((= n 0) 0)
	((even? n) (+ (double b) (fast-multiply b (- n 2))))
	(else (+ b (fast-multiply b (- n 1))))))

(define (fast-multiply-iter b n)
  (define (iter product counter)
    (cond ((<= counter 1) product)
	  ((even? counter) (iter (+ product (double b)) (- counter 2)))
	  (else (iter (+ product b) (- counter 1)))))
  (iter 0 n))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (square q) (* 2 p q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))
