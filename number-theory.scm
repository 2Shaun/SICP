(define (gcd a b)
  (cond ((> b a) (gcd b a))
	((= (modulo a b) 0) b)
	(else (gcd (modulo a b)))))
