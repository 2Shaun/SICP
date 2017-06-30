#lang racket

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (move n from to spare)
  (cond ((= n 1) (print "Move ") (print from) (print " to ") (print to)(newline))
      (else (move (- n 1) from spare to)
            (move 1 from to spare)
            (move (- n 1) spare to from))
      ))
"(define (move-iter n from to spare)
  (define (iter )))"

(move 3 1 3 2)