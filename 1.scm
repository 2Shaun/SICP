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

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2) 3
      (+ n 2))
  )

(define (divides? a b)
  (= (remainder b a) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lb ub)
  (define (iter odd-lb-iter)
    (cond ((<= odd-lb-iter ub)
	   (timed-prime-test odd-lb-iter)
	   (iter (+ odd-lb-iter 2)))))
    (if (even? lb)
	(iter (+ lb 1))
	(iter lb)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (cond ((= (remainder (square (expmod base (/ exp 2) m))
			      m) 1) 0)
	       (else 1)))
	(else (remainder (* base (expmod base (- exp 1) m))
			 m))))
(define (mr-test n)
  (define (try-it a)
    (= (expmod-mr a (- n 1) n) 0))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-prime? n times)
  (cond ((= times 0) true)
	((mr-test n) (mr-prime? n (- times 1)))
	(else false)))

(define (factormod base f m)
  (define (iter i)
    (cond ((<= i f) (display (remainder (* base i) m))
	   (newline)
	   (iter (+ i 1)))
	  (else 0)))
  (iter 1))
	
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (-n 1)))))

(define (fermat-test-iter n)
  (define (try-it a)
    (cond ((<= a n)
	  (display a)
	  (display (= (expmod a n n) a))
	  (newline)
	  (try-it (+ a 1)))))
  (try-it 1))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else (false))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (sum-integers a b)
  (sum indentity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (sum-cubes a b)
  (sum cube a inc b))

;(define (simpsons-rule f a b n)
;  (define (iter k)
;    (if (< k n)
;	(+ (* (power 2 (remainder (power -1 k) 3))
;	    (/ (/ (- b a) n) 3) (f (+ a (* k
;					     (/ (- b a) n)))))
;	   (iter (+ k 1)))
;	0))
;  (iter 0))
;

(define (simpsons-rule-first-order f a b n)
  (define h (/ (- b a) n))
  (define (iter k)
    (if (< k n)
	(+ (*
	    (cond ((or (= k 1) (= k n)) 1)    ; return 1, 2, or 4 for the coefficient
		  ((even? k) 2)
		  (else 4))
	    (/ h 3)                           ; h/3
	    (f (+ a (* k (/ (- b a) n)))))    ; y_k
	   (iter (inc k)))                   ; + (1 || 2 || 4) * h/3 * y_{k+1}
	0))
  (iter 0))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define y (f (+ a (* k (/ (- b a) n)))))
  (define term (* (cond ((or (= k 1) (= k n)) 1)
		     ((even? k) 2)
		     (else 4)) y))
  (* (/ h 3) (sum term a next b)))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter (next a) (term a)))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter (next a) (term a)))

(define (product term a next b)
  (if (< a b)
      (* (term a) (product term (next a) next b))
      (term a)))

(define (pi-approx n)
  (/ (* (* 2 n) (* 2 (+ n 1))) (square (+ (* 2 n) 1))))

(define (accumulate combiner null-value term a next b)
  (cond ((< a b) (combiner (term a) (accumulate combiner null-value term (next a) next b)))
	 ((= a b) (term a))
	 (else null-value)))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (cond ((< a b) (iter (next a) (combiner result (term a))))
	  ((> a b) null-value)
	  (else (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate combiner filter null-value term a next b)
  (cond ((< a b) (combiner
		  (if (filter a) (term a)
		      null-value)
		  (filtered-accumulate combiner filter null-value term (next a) next b)))
	 ((= a b) (if (filter a) (term a)
		      null-value))
	(else null-value)))

(define (rel-prime? n)
  (define (rel-prime?-n i)
    (if (= (gcd n i) 1) true
	false))
  rel-prime?-n)

(define tolerance 0.00000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (display guess)
    (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


(define (cont-frac n d k)
  (define (iter i)
    (if (< i k) (/ (n i) (+ (d i) (iter (+ i 1))))
	(/ (n k) (d k))))
  (iter 1))

(define (cont-frac-iter-recip n d k)
  (define (iter i result)
    (if (> i 0) (iter (- i 1) (+ (/ (n i) result) (d (- i 1))))
	(/ 1 result)))
  (iter k (d k)))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (> i 0) (iter (- i 1) (/ (n i) (+ (d i) result)))
	result))
  (iter (- k 1) (/ (n k) (d k))))

(define (euler-expansion i)
  (if (= (remainder i 3) 2)
      (* 2 (ceiling (/ i 3)))
      1))

(define e (+ 2 (cont-frac (lambda (x) 1.0) euler-expansion 10)))

(define (tan-cf x)
  (/ x (+ 1 (cont-frac (lambda (i) (- (square x))) (lambda (i) (+ (* 2 i) 1)) 100)))) 

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;(define (sqrt x)
;  (newtons-method (lambda (y) (- (square y) x))
;		  1.0))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-iter f n)
  (define (iter composition i)
    (cond ((= i 0) composition)
	  ((even? i) (iter (compose composition (double f)) (- i 2)))
	  (else (iter (compose composition f) (- i 1)))))
  (iter f (- n 1)))

(define (repeated f n)
  (if (> n 1) (compose f (repeated f (- n 1)))
      f))

(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (- x 1)))))

(define (average x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (fourth-root x)
  (fixed-point (double (average-damp (lambda (y) (/ x (cube y))))) 1.0))

(define (fifth-root x r)
  (fixed-point ((repeated average-damp r) (lambda (y) (/ x (expt y 4)))) 1.0))

(define (eigth-root x)
  (fixed-point (repeated-iter (average-damp (lambda (y) (/ x (expt y 7)))) 20) 1.0))

(define (log2 n)
  (/ (log n) (log 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n))) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(define (iterative-improve good-enough? improve)    ; takes in two procedures as arguments (good-enough? & improve)
  (define (iter guess)                              ; return a procedure that takes a guess
    (if (good-enough? guess)                        ; until it is good enough
	guess
	(iter (improve guess))))                    ; and keeps improving its guess
  (lambda (guess) (iter guess)))                 

;(define (sqrt146 x)
;  (define (good-enough? guess)                     there is a binding issue here
;    (lambda (guess) (< (abs (- (square guess) x)) 0.001)))
;  (define (improve guess)
;    (lambda (guess) (/ (+ guess (/ x guess)) 2)))
;  (iterative-improve good-enough? improve) 1.0)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (/ (+ guess (/ x guess)) 2))) 1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- guess (f guess))) 0.00001))
    (lambda (guess) (f guess))) first-guess))

(define (multiply-by x)                             ; currying
  (lambda (y) (* x y)))                             ; evaluate each argument one function at a time
