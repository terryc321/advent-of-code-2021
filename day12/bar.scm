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
  (call-with-input-file "day12/input" (lambda (port)  (read port))))

(define example1
  (call-with-input-file "day12/example1" (lambda (port)  (read port))))

(define example2
  (call-with-input-file "day12/example2" (lambda (port)  (read port))))

(define example3
  (call-with-input-file "day12/example3" (lambda (port)  (read port))))


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
#|
start at start
end at end
any symbol UPPERCASE means it can be visited more than once
lowercase symbol represents a small cave only allow visit once

depth first search start at start 
where we are
location
here
finished? 
|#

(define (big-cave? sym)
  (let ((ascii (char->integer (string-ref (format #f "~a" sym) 0))))
    (< ascii 91)))

(define (small-cave? sym)
  (not (big-cave? sym)))


#|

start at start , not visited anywhere ? or should start be included in visited ?

a - b
A - b
a - B
A - B

think depth first search would find all ways to do this ...?
reason is that not thinking about bi-directional
matching only on first element is a directed only - reason no solutions

something amiss with eq ? 

now allowed to visit a single small cave twice , rest only allowed to visit once

|#
(define (reach loc paths)
  (let ((res '()))
    (do-list path paths
	     (match path
	       ((a b) (cond
		       ((eq? loc a) (set! res (cons b res)))
		       (#t #f)))))
    res))

  
(define recorded-paths '())
(define (reset)
  (set! recorded-paths '()))


(define (seek loc visited paths)
  (format #t "loc= ~a : visited = ~a~%" loc visited)
  (do-list path paths
	   (match path
	     ((a b) (cond
		     ((and (eq? a loc) (eq? b 'end))
		      (let ((completed-path (reverse (cons 'end visited))))
			(format #t "path ~a ~%" completed-path)
			(set! recorded-paths (cons completed-path recorded-paths))))
		     ((and (eq? a loc) (small-cave? b) (not (member b visited)))
		      (seek b (cons b visited) paths))
		     ((and (eq? a loc) (big-cave? b))
		      (seek b (cons b visited) paths))		     
		     (#t #f))))))

(define (bi-direct paths)
  (append paths
	  (map (lambda (pr) (list (second pr)(first pr)))
	       paths)))


(define (test-1)
  (reset)
  (seek 'start '(start) (bi-direct example1)))

(define (test paths)
  (reset)
  (seek 'start '(start) (bi-direct paths))
  (pp recorded-paths)
  (length recorded-paths))


(define (pass-1)
  (test input))


;; ------------ unit tests -------------

;; ------------ test cases -------------


;; ----------- results ------------------

#|


chicken time an expression
,t (expr)

(pass-1)
3421 paths


compile a file in chicken scheme
csc -o fun fun.scm

|#
