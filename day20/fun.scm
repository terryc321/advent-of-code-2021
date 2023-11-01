
;;(import (lsp-server))

(import scheme)
(import (chicken process-context))
;; change-directory

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
;; (current-directory)
;; (change-directory "day20")

;;(define input  (call-with-input-file "example" (lambda (port)  (read port))))
(define input  (call-with-input-file "input" (lambda (port)  (read port))))

#|
image enhancement algorithm  -- a string
 string-ref s 0
contains chars #\#  or #\.  char=?
# meaning lit
. meaning dark

|#
(define iea (car input))

#|

image is supposed to be infinite
input-image is a list of strings 100 elements long with strings 100 chars in length

|#
(define input-image (cadr input))

(define (1- x) (- x 1))

(define min-x 0)
(define max-x 0)
(define min-y 0)
(define max-y 0)


;; hash-table-set! h k v
;; hash-table-ref h k
(define (convert img)
  (let ((hash (make-hash-table))
	(height (length img))
	(width (string-length (car img))))
    (set! min-x 0)
    (set! min-y 0)
    (set! max-x (- width 1))
    (set! max-y (- height 1))
    (letrec ((foo (lambda (x y)
		    (cond
		     ((> y (1- height)) hash)
		     ((> x (1- width)) (foo 0 (+ y 1)))
		     (#t
		      (let* ((val (string-ref (list-ref img y) x))
			     (pt (list x y)))
			(format #t "processing ~a,~a ~%" x y)
			(cond
			 ((char=? val #\.) #f)
			 ((char=? val #\# )
			  (hash-table-set! hash pt #t))
			 (#t (error (list "convert.img.fail" val))))
			(foo (+ x 1) y)))))))
      (foo 0 0)
      hash)))


(define init-hash (convert input-image))

#|
given a hash , apply the image enhancement algorithm
min-x max-x
min-y max-y are only indicators where last image

a b c
d e f
g h i

|#
(define (transform ha x y)
  (let ((a (list (+ x -1)(+ y -1)))  (b (list x (+ y -1)))   (c (list (+ x 1) (+ y -1)))
	(d (list (+ x -1)(+ y 0)))  (e (list x (+ y 0)))   (f (list (+ x 1) (+ y 0)))
	(g (list (+ x -1)(+ y 1)))  (h (list x (+ y 1)))   (i (list (+ x 1) (+ y 1))))
    (let ((pa (hash-table-ref ha a (lambda () #f)))
	  (pb (hash-table-ref ha b (lambda () #f)))
	  (pc (hash-table-ref ha c (lambda () #f)))
	  (pd (hash-table-ref ha d (lambda () #f)))
	  (pe (hash-table-ref ha e (lambda () #f)))
	  (pf (hash-table-ref ha f (lambda () #f)))
	  (pg (hash-table-ref ha g (lambda () #f)))
	  (ph (hash-table-ref ha h (lambda () #f)))
	  (pi (hash-table-ref ha i (lambda () #f))))
      (let ((bin-str (format #f "~a~a~a~a~a~a~a~a~a"
			     (if pa 1 0)
			     (if pb 1 0)
			     (if pc 1 0)
			     (if pd 1 0)
			     (if pe 1 0)
			     (if pf 1 0)
			     (if pg 1 0)
			     (if ph 1 0)
			     (if pi 1 0))))
	(let ((index (string->number bin-str 2)))
	  (let ((ch (string-ref iea index)))
	    (cond
	     ((char=? ch #\#) #t)
	     (#t #f))))))))


#|

|#
(define (bar ha min-x max-x min-y max-y)
  (let ((hash (make-hash-table)))
    (letrec ((foo (lambda (x y)
		    (cond
		     ((> y max-y) hash)
		     ((> x max-x) (foo min-x (+ y 1)))
		     (#t
		      (let* ((val (transform ha x y))
			     (pt (list x y)))
			;;(format #t "processing ~a,~a ~%" x y)
			(cond
			 ((not val) #f)
			 (#t
			  (hash-table-set! hash pt #t)))
			(foo (+ 1 x) y)))))))
      (foo min-x min-y)
      hash)))


(define (tidy ha min-x max-x min-y max-y)
  (letrec ((foo (lambda (x y)
		  (cond
		   ((< y min-y) #t)		    
		   ((> x max-x)
		    (format #t "~%")
		    (foo min-x (- y 1)))
		   (#t
		    (let* ((val (hash-table-ref ha (list x y) (lambda () #f))))
		      (cond
		       ((not val) (format #t "."))
		       (#t (format #t "#")))
		      (foo (+ 1 x) y)))))))
    (foo min-x max-y)))



(define (pass-1)
  (let ((lo-x (- min-x 20))
	(lo-y (- min-y 20))
	(hi-x (+ max-x 20))
	(hi-y (+ max-y 20)))
    (tidy init-hash lo-x hi-x lo-y hi-y)
    (let ((ha1 (bar init-hash lo-x hi-x lo-y hi-y)))
      (format #t "~%~%")
      (tidy ha1 lo-x hi-x lo-y hi-y))))


(define (pass-2)
  (let ((lo-x (- min-x 20))
	(lo-y (- min-y 20))
	(hi-x (+ max-x 20))
	(hi-y (+ max-y 20)))
    (tidy init-hash lo-x hi-x lo-y hi-y)
    (let ((ha1 (bar init-hash lo-x hi-x lo-y hi-y)))
      (format #t "~%~%")
      (tidy ha1 lo-x hi-x lo-y hi-y)
      (let ((ha2 (bar ha1 lo-x hi-x lo-y hi-y)))
	(format #t "~%~%")
	(tidy ha2 (+ 3 lo-x) (- hi-x 3) (+ 3 lo-y) (- hi-y 3))))))


(define sol-str  (call-with-input-file "slide2" (lambda (port)  (read port))))

(define (1+ x) (+ x 1))

(define sol
  (let ((len (string-length sol-str))
	(count 0))
    (letrec ((foo (lambda (n)
		  (cond
		   ((>= n len) count)
		   (#t (let ((ch (string-ref sol-str n)))
			 (when (char=? #\# ch)
			   (set! count (1+ count)))
			 (foo (+ n 1))))))))
      (foo 0))))
    
#|


#;1979> sol
5486
#;2103> 

5486 are lit after two iterations of the image algorithm

accepted answer


|#


#|

chicken time an expression
,t (expr)

compile a file in chicken scheme
csc -o fun fun.scm

|#
