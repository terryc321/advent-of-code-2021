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

(define initial '(V C O P V N K P F O O V P V S B K C O F ))

(define transitions '((N O   K)
		      (P O   B)
		      (H S   B)
		      (F P   V)
		      (K N   S)
		      (H V   S)
		      (K C   S)
		      (C S   B)
		      (K B   V)
		      (O B   V)
		      (H N   S)
		      (O K   N)
		      (P C   H)
		      (O O   P)
		      (H F   S)
		      (C B   C)
		      (S B   V)
		      (F N   B)
		      (P H   K)
		      (K H   P)
		      (N B   F)
		      (K F   P)
		      (F K   N)
		      (F B   P)
		      (F O   H)
		      (C V   V)
		      (C N   P)
		      (B N   N)
		      (S C   N)
		      (P B   K)
		      (V S   N)
		      (B P   P)
		      (C K   O)
		      (P S   N)
		      (P F   H)
		      (H B   S)
		      (V N   V)
		      (O S   V)
		      (O C   O)
		      (B B   F)
		      (S K   S)
		      (N F   F)
		      (F S   S)
		      (S N   N)
		      (F C   S)
		      (B H   N)
		      (H P   C)
		      (V K   F)
		      (C C   N)
		      (S V   H)
		      (S O   F)
		      (H H   C)
		      (P K   P)
		      (N V   B)
		      (K S   H)
		      (N P   H)
		      (V O   C)
		      (B K   V)
		      (V V   P)
		      (H K   B)
		      (C F   B)
		      (B F   O)
		      (O V   B)
		      (O H   C)
		      (P P   S)
		      (S P   S)
		      (C H   B)
		      (O F   F)
		      (N K   F)
		      (F V   F)
		      (K P   O)
		      (O P   O)
		      (S S   P)
		      (C P   H)
		      (B O   O)
		      (K K   F)
		      (H C   N)
		      (K O   V)
		      (C O   F)
		      (N C   P)
		      (O N   P)
		      (K V   C)
		      (B V   K)
		      (H O   F)
		      (P V   H)
		      (V C   O)
		      (N H   B)
		      (P N   H)
		      (V P   O)
		      (N S   N)
		      (N N   S)
		      (B S   H)
		      (S H   P)
		      (V B   V)
		      (V H   O)
		      (F H   K)
		      (F F   H)
		      (S F   N)
		      (B C   H)
		      (V F   P)
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

(define result (example))

(define (counts)
  (let ((xs result)
	(polymers (find-polymers))
	(cts '()))
    (do-list polymer polymers
	     (let ((num (length (filter (lambda (x) (eq? x polymer)) xs))))
	       (set! cts (cons (list polymer num) cts))))
    (sort cts (lambda (x y) (< (second x)( second y))))))

(define (pass-1)
  (let ((cts (counts)))
    (let ((low (second (first cts)))
	  (high (second (last cts))))
      (- high low))))


#|

#;279> (counts)
((C 574) (F 897) (K 1048) (V 1085) (H 1415) (B 1618) (S 2969) (O 3104) (N 3322) (P 3425))

(pass-1)
2851

|#

	     
;; ------------ unit tests -------------

;; ------------ test cases -------------

;; ----------- results ------------------

#|


chicken time an expression
,t (expr)


compile a file in chicken scheme
csc -o fun fun.scm

|#
