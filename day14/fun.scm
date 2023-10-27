;;(import (lsp-server))

(import scheme)
(import (chicken io)) ;; read-line
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
  (call-with-input-file "day14/input" (lambda (port)  (read port))))

(define example
  (call-with-input-file "day14/example" (lambda (port)  (read port))))


#|

(define example2
  (call-with-input-file "day12/example2" (lambda (port)  (read port))))

(define example3
  (call-with-input-file "day12/example3" (lambda (port)  (read port))))
|#


#|
(define (forever port lines)
  (let ((got (read-line port)))
    ;;(format #t "read-line ~a ~%" got)
    (cond
     ((eof-object? got) (reverse lines))
     (#t (forever port (cons got lines))))))


(define input-lines
  (call-with-input-file "day10/input" (lambda (port)
					(forever port '()))))

(define input-lines2
  (call-with-input-file "day10/input2" (lambda (port)
					(forever port '()))))

|#

;; (define input2
;;   (call-with-input-file "day10/input2" (lambda (port)  (read port))))

;; ------------ puzzle ---------------------------

;; for example

(define initial '(N N C B))

(define transitions '((C H   B)
		      (H H   N)
		      (C B   H)
		      (N H   C)
		      (H B   C)
		      (H C   B)
		      (H N   C)
		      (N N   C)
		      (B H   H)
		      (N C   B)
		      (N B   B)
		      (B N   B)
		      (B B   N)
		      (B C   B)
		      (C C   N)
		      (C N   C)
		      ))

(define the-pairs '())

(define (pair a b)
  (call/cc (lambda (escape)
	     (do-list pr transitions
		      (cond
		       ((and (eq? a (first pr))
			     (eq? b (second pr)))
			(set! the-pairs (cons (third pr) the-pairs))
			(escape (third pr)))
		       (#t #f)))
	     (error (list "no transition match on pair" a b)))))


(define (pairs state)
  (cond
   ((null? (cdr (cdr state))) ;;last pair
    (pair (first state) (second state)))
   (#t
      (pair (first state) (second state))
      (pairs (cdr state)))))

#|
weave a list original sequence N N C B
with generated from pairs N N , N C , C B
one from xs
one from ys
and repeat until run out of ys in which case just final xs
N _ N _ C _ B    : xs
  /\  /\  /\     : ys 
should always be one more xs than ys as gaps between xs generate the ys 

|#
(define (munge xs ys zs)
  (cond
   ((null? xs) (reverse zs))
   ((null? ys) (munge '() '() (cons (car xs) zs)))
   (#t (munge (cdr xs) (cdr ys) (cons (car ys) (cons (car xs) zs))))))

(define (test-1 state)
  (set! the-pairs '())
  (pairs state)
  (set! the-pairs (reverse the-pairs))
  (munge state the-pairs '()))

(define (steps state n lim)
  (format #t "step ~a ~% ~a ~%" n state)
  (cond
   ((= n lim)
    (format #t "~a~%" state)
    state)
   (#t (steps (test-1 state) (+ n 1) lim))))

(define (count-polymers2 xs asc)
  (cond
   ((null? xs) asc)
   (#t (let ((s (car xs)))
	 (let ((asv (assoc s asc)))
	   (cond
	    (asv ;; increment the value
	     (set-car! (cdr asv) (+ 1 (car (cdr asv))))
	     )
	  (#t (count-polymers2 (cdr xs) (cons (list s 1) asc)))))))))

(define (count-polymers xs)
  (count-polymers2 xs '()))


(define (find-polymers2 xs known)
  (cond
   ((null? xs) known)
   ((member (car xs) known)
    (find-polymers2 (cdr xs) known))
   (#t (find-polymers2 (cdr xs) (cons (car xs) known)))))

(define (find-polymers)
  (let ((known (find-polymers2 initial '())))
    (find-polymers2 (apply append transitions) known)))


(define (example)
  (steps initial 0 10))



;; ------------ unit tests -------------

;; ------------ test cases -------------

;; ----------- results ------------------

#|


chicken time an expression
,t (expr)


compile a file in chicken scheme
csc -o fun fun.scm

|#
