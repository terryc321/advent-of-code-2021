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
  (call-with-input-file "day8/input" (lambda (port)  (read port))))

;; break input by newlines
;; then split by space
;;
(define (disgard-before-bar xs)
  (cond
   ((null? xs) xs)
   ((string= (car xs) "|") (cdr xs))
   (#t (disgard-before-bar (cdr xs)))))

(define temper
  (map disgard-before-bar (map string-split (string-split input "\n"))))


(define (count-1478 xs)
  (define (count-em ys n)
    (cond
     ((null? ys) n)
     (#t (let* ((s (car ys))
		(slen (string-length s)))
	   (cond
	    ((or (= slen 2) ;; digit 1
		 (= slen 4) ;; digit 4
		 (= slen 3) ;; digit 7
		 (= slen 7) ;; digit 8
		 )
	     (count-em (cdr ys) (+ n 1)))
	    (#t (count-em (cdr ys) n)))))))
  (count-em xs 0))


#|
(define input2
  (call-with-input-file "./input2" (lambda (port) (read port))))
|#

;; ------------ puzzle ---------------------------

;; ------------ unit tests -------------

;; ------------ test cases -------------

(define (example-1)
  #t)

(define (test-1)
  (apply + (map count-1478 temper)))

 
(define (test-2)
  #t)

;; ----------- results ------------------

#|

chicken time an expression
,t (expr)

csc -o fun fun.scm

part - 1
,t (test-1)
0.003s CPU time, 1749/229 mutations (total/tracked), 0/11 GCs (major/minor), maximum live heap: 1.23 MiB
261

261 

|#
