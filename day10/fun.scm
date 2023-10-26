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
  (call-with-input-file "day10/input" (lambda (port)  (read port))))


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



;; (define input2
;;   (call-with-input-file "day10/input2" (lambda (port)  (read port))))

;; ------------ puzzle ---------------------------
#|
grammar
[ ]
( )
{ }
< >

parse a string with malformed pieces
(string-ref s n)  where n ... 0 to (string-length) - 1 

think of using a stack
when stack is empty - parse succeeded
when meet { - stack gains {
when meet [ - stack gains [
when meet ( - stack gains (
when meet < - stack gains <

when meet } - stack drops {
when meet ] - stack drops [
when meet ) - stack drops (
when meet > - stack drops <

|#

(define (parse-curve s n)
  #t)

(define (parse-curly s n)
  #t)

(define (parse-square s n)
  #t)

(define (parse-angle s n)
  #t)

#|
(define-macro (push x)
  `(set! stack (cons ,x stack)))

(define-macro (pop)
  `(let ((top (car stack)))
     (set! stack (cdr stack))
     top))

(define-syntax (push x)
  (macro-rules 
  `(set! stack (cons ,x stack)))
|#

(define (parse s)
  (letrec ((score (lambda (ch)
		    (cond
		     ((char=? ch #\) ) 3)
		     ((char=? ch #\] ) 57)
		     ((char=? ch #\} ) 1197)
		     ((char=? ch #\> ) 25137)
		     (#t (error (list "cannot score character" ch)))))))
    (call/cc (lambda (corrupted)
	       (let ((stack '()))
		 (do-for n (0 (string-length s) 1)
			 (let ((ch (string-ref s n)))
			   (cond
			    ((char=? ch #\[ ) (set! stack (cons ch stack)))
			    ((char=? ch #\{ ) (set! stack (cons ch stack)))
			    ((char=? ch #\( ) (set! stack (cons ch stack)))
			    ((char=? ch #\< ) (set! stack (cons ch stack)))
			    ((char=? ch #\] )
			     (cond
			      ((and (pair? stack) (char=? (car stack) #\[ ))
			       (set! stack (cdr stack)))
			      (#t (corrupted (score ch)))))
			    ((char=? ch #\} )
			     (cond
			      ((and (pair? stack) (char=? (car stack) #\{ ))
			       (set! stack (cdr stack)))
			      (#t (corrupted (score ch)))))
			    ((char=? ch #\) )
			     (cond
			      ((and (pair? stack) (char=? (car stack) #\( ))
			       (set! stack (cdr stack)))
			      (#t (corrupted (score ch)))))			  
			    ((char=? ch #\> )
			     (cond
			      ((and (pair? stack) (char=? (car stack) #\< ))
			       (set! stack (cdr stack)))
			      (#t (corrupted (score ch)))))
			    (#t
			     (error (list "unrecognised character" 'ch ch 's s 'n n))))
			   ;;(format #t "i = ~a ~%" ch)
			   )))
	       0))))



#|
    (cond
     ((null? stack) (values #t '()))
     (#t (values #f stack)))))))
|#




;; ------------ unit tests -------------

;; ------------ test cases -------------

(define (example-1)
  #t)

(define (debug-1)
  #t)

(define (test-1)
  (apply + (map parse input-lines)))


 
(define (test-2)
  #t)

;; ----------- results ------------------

#|

chicken time an expression
,t (expr)

#;6> ,t (test-1)
0.009s CPU time, 11681/3735 mutations (total/tracked), 0/94 GCs (major/minor), maximum live heap: 1.03 MiB
394647


compile a file in chicken scheme
csc -o fun fun.scm

|#
