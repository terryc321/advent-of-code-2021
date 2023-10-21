;;(import (lsp-server))

(import scheme)
(import (chicken format))
(import (chicken sort))
(import (chicken pretty-print))
(define pp pretty-print)

;;(import (chicken doc))
;; documentation

;; debugging macro expander
;; debugger

(import procedural-macros)
(import regex)

(import simple-md5)

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

(define example '(16 1 2 0 4 2 7 1 2 14))

(define input
  (call-with-input-file "./input" (lambda (port)  (read port))))

#|
(define input2
  (call-with-input-file "./input2" (lambda (port) (read port))))
|#

;; ------------ puzzle ---------------------------

#|
start with heuristic that aligned should be between min and max of values provided
just a guess

|#
(define (fuel a b)
  (let ((dist (abs (- a b))))
    (/ (* dist (+ dist 1)) 2)))


;; ------------ unit tests -------------

;; ------------ test cases -------------

(define (example-1)
  (let ((xs example))
    (let* ((lo (apply min xs))
	   (hi (apply max xs)))
      (do-for v (lo hi 1)
	      (let ((cost (apply + (map (lambda (n) (fuel n v)) xs))))
		(format #t "v = ~a : cost = ~a~%" v cost))))))

(define (test-1)
  (let ((xs input)
	(best '(cost-> 99999999999 val-> fake)))
    (let* ((lo (apply min xs))
	   (hi (apply max xs)))
      (do-for v (lo hi 1)
	      (let ((cost (apply + (map (lambda (n) (fuel n v)) xs))))
		(cond
		 ((< cost (second best))
		  (set! best (list 'cost-> cost 'val-> v)))
		 (#t #f))
		(format #t "v = ~a : cost = ~a~%" v cost)))
      best)))

 
(define (test-2)
  #t)

;; ----------- results ------------------

#|

chicken time an expression
,t (expr)

csc -o fun fun.scm

part - 1
0.578s CPU time, 0.016s GC time (major), 3992933/1670370 mutations (total/tracked), 9/6281 GCs (major/minor), maximum live heap: 1.05 MiB
(cost-> 354129 val-> 361)

fuel expended 354129

part - 2

with a new fuel formula
n = dist where 
 n (n+1) / 2 

,t (test-1) 
1.204s CPU time, 0.021s GC time (major), 4066974/1877649 mutations (total/tracked), 13/16131 GCs (major/minor), maximum live heap: 1.05 MiB
(cost-> 98905973 val-> 494)



|#

