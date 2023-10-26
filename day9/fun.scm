;;(import (lsp-server))

(import scheme)
(import (chicken format))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

;;(import (chicken doc))
;; documentation

;; debugging macro expander
;; debugger

(import procedural-macros)
(import regex)

(import simple-md5)

(import srfi-13)
;;srfi-13 for string=

(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val

;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors

;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(import matchable)

(import simple-loops)
;; do-list

(import vector-lib)
;; vector-copy


(define input
  (call-with-input-file "day9/input3" (lambda (port)  (read port))))

(define input2
  (call-with-input-file "day9/input2" (lambda (port)  (read port))))

;; ------------ puzzle ---------------------------
#|
access input as a grid 100 x 100
vector-ref v i
0 to 99 inclusive x
0 to 99 inclusive y

determine where the low points are on the grid provided

upper left corner point - only right and down
upper right corner point - only left and down
upper line point - only left right down

100 x 100 thats 10,000 squares 
a square can be eliminated if its neighbour square 

|#

(define (onboard x y)
  (and (>= x 0) (<= x 99)
       (>= y 0) (<= y 99)))

(define (grid x y)
  (assert (onboard x y))
  (vector-ref (vector-ref input y) x))

;; something to record if low spot found
(define (make-grid w h)
  (let ((vert (make-vector h #t)))
    (do-for y (0 h 1)
	    (let ((horz (make-vector w #t)))
	      (vector-set! vert y horz)))
    vert))

(define know (make-grid 100 100))

(define (elim x y)
  (vector-set! (vector-ref know y) x #f))


(define-macro (peek-grid escape val x y dx dy)
  `(when (onboard ,dx ,dy)
     (let ((val2 (grid ,dx ,dy)))
       (cond
	((< val2 ,val) (elim ,x ,y) (,escape #t))
	(#t #f)))))

#|
eliminate all points that are higher than one of its neighbours
only points left will be low points
unless they are connected like area of zeros somewhere...

added bug-001 to eliminate positions with maximum value of 9 - ie a high spot
since a high spot cannot be a low spot 

|#
(define (peek x y)
  (call/cc (lambda (escape)
	     (when (onboard x y)
	       (let ((val (grid x y)))
		 (when (= val 9) ;; bug-001
		   (elim x y))
		 (peek-grid escape val x y x (- y 1))
		 (peek-grid escape val x y x (+ y 1))
		 (peek-grid escape val x y (+ x 1) y)
		 (peek-grid escape val x y (- x 1) y))))))

#|
iterate over all squares
any square with neighbours that are below current square are
eliminated
|#
(define (seek)
  (do-for x (0 100 1)
	  (do-for y (0 100 1)
		  (peek x y))))


;; ------------ unit tests -------------

;; ------------ test cases -------------

(define (example-1)
  #t)

(define (debug-1)
  (seek)
  (let ((sum 0))
    (do-for y (0 100 1)
	    (do-for x (0 100 1)
		    (cond
		     ((vector-ref (vector-ref know y) x)
		      (format #t "~a" (grid x y)))
		     (#t (format #t "."))))
	    (format #t "~%"))))


(define (test-1)
  (seek)
  (let ((sum 0))
    (do-for x (0 100 1)
	    (do-for y (0 100 1)
		    (cond
		     ((vector-ref (vector-ref know y) x)
		      (let ((val (vector-ref (vector-ref input y) x)))
			(set! sum (+ sum (+ 1 val)))
			(format #t "value ~a ~%" (+ 1 val))))
		     (#t #f))))
    sum))





 
(define (test-2)
  #t)

;; ----------- results ------------------

#|

chicken time an expression
,t (expr)

,t (test-1)

0.128s CPU time, 60649/1691 mutations (total/tracked), 0/756 GCs (major/minor), maximum live heap: 1.3 MiB
545

|#


#|

no compilation required 

csc -o fun fun.scm

|#
