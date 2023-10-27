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
  (call-with-input-file "day13/input" (lambda (port)  (read port))))

#|
(define example1
  (call-with-input-file "day12/example1" (lambda (port)  (read port))))

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
#|

here are the folds 

(fold  x 655)
(fold  y 447)
(fold  x 327)
(fold  y 223)
(fold  x 163)
(fold  y 111)
(fold  x 81)
(fold  y 55)
(fold  x 40)
(fold  y 27)
(fold  y 13)
(fold  y 6)

|#

(define points input)


(define (fold-left x xline)
  (cond
   ((> x xline)  (+ xline (- xline x)))
   (#t x)))

(define (fold-up y yline)
  (cond
   ((> y yline)  (+ yline (- yline y)))
   (#t y)))

(define (fold-x xline)
  (lambda (pr)
    (let ((x (first pr))
	  (y (second pr)))
      (list (fold-left x xline) y))))

(define (fold-y yline)
  (lambda (pr)
    (let ((x (first pr))
	  (y (second pr)))
      (list x (fold-up y yline)))))

(define (remove-dups xs)
  (letrec ((foo (lambda (xs ys)
		  (cond
		   ((null? xs) ys)
		   ((member (car xs) ys)
		    (foo (cdr xs) ys))
		   (#t (foo (cdr xs) (cons (car xs) ys)))))))
    (foo xs '())))

(define (test-1)
  (let ((fold-x-655 (fold-x 655)))
    (remove-dups (map fold-x-655 points))))



(define (test-2)
  (let ((points input)
	(folds `((fold  x 655)
		 (fold  y 447)
		 (fold  x 327)
		 (fold  y 223)
		 (fold  x 163)
		 (fold  y 111)
		 (fold  x 81)
		 (fold  y 55)
		 (fold  x 40)
		 (fold  y 27)
		 (fold  y 13)
		 (fold  y 6))))
    (do-list fold folds
	     (match fold
	       (('fold 'x n)
		;;(format #t " x axis fold ~a : ~a~%" fold n)
		(let ((fold-x-specific (fold-x n)))
		  (set! points (remove-dups (map fold-x-specific points)))))
	       (('fold 'y n)
		;;(format #t " y axis fold ~a : ~a~%" fold n)
		(let ((fold-y-specific (fold-y n)))
		  (set! points (remove-dups (map fold-y-specific points)))))
	       ( _ (error (list "test-2 mis-match")))))
    points))

(define (range-2)
  (let* ((res (test-2))
	 (x-values (map first res))
	 (y-values (map second res)))
    (list (apply min x-values)
	  (apply min y-values)
	  (apply max x-values)
	  (apply max y-values))))


;; visual decode of the test-2 routine
(define (display-2)
  (let* ((res (test-2)))
    (format #t "~%")
    (do-list y (iota 6)
	     (do-list x (iota 39)
		      (let ((pr (list x y)))
			(cond
			 ((member pr res) (format #t "#"))
			 (#t (format #t "_")))))
	     (format #t "~%"))))



;; ------------ unit tests -------------

;; ------------ test cases -------------

(define (pass-1)
  (length (test-1)))

;; ----------- results ------------------

#|

#;899> (display-2)

#__#_####_####_####__##__#__#__##____##
#__#_#____#_______#_#__#_#__#_#__#____#
#__#_###__###____#__#____#__#_#_______#
#__#_#____#_____#___#____#__#_#_______#
#__#_#____#____#____#__#_#__#_#__#_#__#
_##__####_#____####__##___##___##___##_

#;994> UEFZCUCJ
??



chicken time an expression
,t (expr)

0.016s CPU time, 1766/762 mutations (total/tracked), 0/82 GCs (major/minor), maximum live heap: 1.38 MiB
669

669 points after fold along x = 655

compile a file in chicken scheme
csc -o fun fun.scm

|#
