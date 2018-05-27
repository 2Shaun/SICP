(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

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

;(print-point (midpoint (make-segment (make-point 1 2) (make-point 2 3))))

(define (distance seg)
  (sqrt (+ (square (- (x-point (p1 seg)) (x-point (p2 seg))))
	   (square (- (y-point (p1 seg)) (y-point (p2 seg)))))))

(define (rectangle seg1 seg2)  ; doesn't validate that seg1 is touching seg2 (they must share a point
  (cons seg1 seg2))

(define (rectangle-p p1 corner p2)
  (cons (make-segment p1 corner) (make-segment corner p2)))

;(define (length rect)
;  (distance (car rect)))

;(define (width rect)
;  (distance (cdr rect)))

(define (area rect)
  (* (length rect) (width rect)))

(define (perimeter rect)
  (+ (* 2 (length rect)) (* 2 (width rect))))

;(define l (make-segment (make-point 1 1) (make-point 1 3)))

;(define w (make-segment (make-point 1 3) (make-point 4 3)))

;(define r (rectangle l w))

;(define rp (rectangle-p (make-point 1 1) (make-point 1 3) (make-point 4 3)))

;(display (area r))
;(newline)
;(display (area rp))
;(newline)
;(display (perimeter r))
;(perimeter r)

; THESE WERE CAUSING A MAJOR HEADACHE
; LISTS WERE 'not applicable'
;(define (cons x y)
;  (lambda (m) x y))

;(define (car z)
;  (z (lambda (p q) p)))

(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

; when dealing with recursion
; you need to pay close attention to data types
; return types, argument types, etc.
; car inputs a list and outputs a list element
; cdr inputs a list and outputs a list
; append inputs two lists and outputs a list
; it's important that were 'going down' the recursion tree first
; we go down to the last element and that is appended on the left

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (reverse l)
(if (= (length l) 1)
l
(append (reverse (cdr l)) (list (car l))))) 

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (= (length coin-values) 0))

(define (same-parity x . y)
  (if (= (length y) 0)
      y
      (if (= (remainder x 2) (remainder (car y) 2))
	  (append (list (car y)) (same-parity x (cdr y)))
	  (same-parity x (cdr y)))))


(define (same-parity x . y)
  (define (same-parity-iter current-y parity-list)
    (if (> (length current-y) 0)
        (if (= (remainder x 2) (remainder (car current-y) 2))
            (same-parity-iter (cdr current-y) (append (list (car current-y)) parity-list))
            (same-parity-iter (cdr current-y) parity-list))
        parity-list))
  (same-parity-iter y (list)))

;(define (map proc items)
;  (if (null? items)
;      (list)
;      (cons (proc (car items))
;	    (map proc (cdr items)))))


(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

; the issue has to do with types
; answer is going to have pairs passed in (lists of size 2)
; cons then creates a pair of a pair and an integer
; using append, we can pass the integer as a singleton (list of size 1)
; and append the two lists
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer
		    (list (square (car things)))))))
  (iter items (list)))

(define (for-each proc lst)
  (define (for-each-iter current-lst)
    (cond ((> (length current-lst) 0) (proc (car current-lst))
	   (for-each-iter (cdr current-lst)))
	  ((= (length current-lst) 0) #t)))
  (for-each-iter lst))

(define l (list 1 3 (list 5 7) 9))

; (car(cdr(car(cdr(cdr l)))))

(define l (list (list 7)))

; (car (car l))

; notice that the outer list is two items
; 1 and the next list
; cdr will give us a singleton
; the only element being the next list
; you can think of a (car (cdr 
; as chopping off the far left (1
; we need 6 of those

(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

; pretty complicated but seems to work
; want to write it using map

; deep-reverse accepts a list and returns a list
(define (deep-reverse l)
  (cond ((null? l) l)
	((= (length l) 1)
	 (if (pair? (car l)) (list (deep-reverse (car l))) ; when I append this list
	     (list (car l))))                              ; the inner list is preserved, outer list not
	(else (append (deep-reverse (cdr l)) (deep-reverse (list (car l))))))) 

; fringe accepts a list and returns a list
; notice that if I call append on two singletons,
; it will return a list of numbers (non-lists)
(define (fringe l)
  (cond ((null? l) l)
	((= (length l) 1) ; keep applying fringe until (list (car l)) is a singleton
	 (if (pair? (car l)) (fringe (car l))
	     (list (car l))))
	(else (append (fringe (list (car l))) (fringe (cdr l))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; these seem like the only two methods we need to change
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define m1 (make-mobile (make-branch 2 5) (make-branch 2 5)))

(define m2 (make-mobile (make-branch 2 5)
			(make-branch 2 (make-mobile (make-branch 2 5)
						    (make-branch 2 5)))))
(define m3 (make-mobile (make-branch 1 (make-mobile
					(make-branch 1 (make-mobile
							(make-branch 1 1) (make-branch 1 1)))
					(make-branch 1 2)))
			(make-branch 2 2)))
					

;(+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile))))
(define (total-weight mobile)
  (if (null? mobile) 0
      (if (not (pair? mobile)) mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile)))))))


;(= (* (branch-length (left branch)) (total-weight (branch-structure (left-branch mobile))))
;   (* (branch-length (right branch)) (total-weight (branch-structure (right-branch mobile)))))
;(and (balanced? (branch-structure (left-branch mobile)))
;     (balanced? (branch-structure (right-branch mobile))))
;(pair? (branch-structure (left-branch mobile)))

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (and (if (not (pair? (branch-structure (left-branch mobile)))) #t
	   (balanced? (branch-structure (left-branch mobile))))
       (if (not (pair? (branch-structure (right-branch mobile)))) #t
	   (balanced? (branch-structure (right-branch mobile))))
       (= (torque (left-branch mobile))
	  (torque (right-branch mobile)))))

(define (square-tree t)
  (cond ((null? t) t)
	((pair? t) (cons (square-tree (car t)) (square-tree (cdr t))))
	(else (* t t))))

(define t1 (list (list 1 2) (list 3 4) (list 5 6)))

(define (tree-map proc t)
  (cond ((null? t) t)
	((pair? t) (cons (tree-map t) (tree-map t)))
	(else (proc t))))

; in the book it has (list nil)
; nil = (list)
; but (list nil) DOES NOT EQUAL (list)
; set of empty set DOES NOT EQUAL empty set
; (()) DOES NOT EQUAL ()
(define (wrong-subsets s)
  (if (null? s)
      ; if rest is null, there is nothing to map over
      ; then, in each level of the stack, rest is null
      (list)
      (let ((rest (wrong-subsets (cdr x))))
	;(append () ()) -> ()
	;by the base case of append
	(append rest (map (lambda (x) (append (list (car s)) x)) rest)))))


; if you have (1 2 3)
; the subsets of (2 3)
; (() (3) (2) (2 3))
; are valid subsets of (1 2 3)
; but they are all missing 1
; notice the valid subsets of (3)
; (() (3))
; are contained in the subsets of (2 3)
; top level of stack:
; (car s): 3
; rest: (())
; (() (3))
; (car s): 2
; rest: (() (3))
; (() (3) (2) (2 3))
(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;(define (map p sequence)
;  (accumulate (lambda (x y) (append (list (p x)) y)) (list) sequence))

; (accumulate cons <??> <??>)
; n.b. think of the box-and-pointer representation of (cons 1 (list 2 3))
; [_|_]->[_|_]->[_|_]->x
;  1      2      3
; the element in the first box of the pair is a 1
; the element in the second box of the pair is a list
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length l)
  (accumulate (lambda (x y) (+ 1 y)) 0 l))

; accumulate op init sequence
; (op (car sequence) (accumulate op init (cdr of sequence)))
; op: (lambda (this-coeff higher-terms) <??>)
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))
;(accumulate <??> <??> (map <??> <??>))
; applying fringe to every tree in the tree creates a list of lists (of elements, not lists)
; I think of it as flattening every tree in the tree
; adding up the lengths of all of these lists gives the total number of leaves
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 0 (map (lambda (x) (length (fringe x))) t)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (scale-vector s v)
  (map (lambda (x) (* s x)) v))

; if map takes in a list of elements, it can apply a one argument procedure to all of them
; if map takes in 2 lists of elements, it can apply a two argument procedure to all of them
					; producing one list of the return values

; (map <??> m)
; matrices are a list of row vectors (as lists)
; for each row
(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

; (accumulate-n <??> <??> mat)
; accumulate-n brings together all of the accumulations of each row using cons (this is built-in)
; our accumulations cons together all of the first elements of each row (this was passed in as an arg)
(define (transpose m)
  (accumulate-n cons (list) m))

; (define (matrix-*-matrix m n)
; (let ((cols (transpose n)))
; (map <??> m)))

; this outputs the diagonal 
; you get a list containing the dot product of the 1st row of m and the 1st column of n
; the dot product of the 2nd row of m and the 2nd column of n
; and so on...
;(define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))
;    (map dot-product cols m)))

; I quickly lose understanding of these algorithms, despite having written them
; I think it's because there are so many ways to think of matrix multiplication
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (transpose (map (lambda (nj) (matrix-*-vector m nj)) cols))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-vector cols mi)) m)))

(define m1 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define v1 (list 2 2 2))

(define (test-dot x y . z)
  (pair? z))


