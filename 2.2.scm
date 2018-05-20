(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))
