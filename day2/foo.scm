


(import scheme)
(import (chicken format))
(import (chicken sort))

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

(define input
  (call-with-input-file "./input" (lambda (port)
				       (read port))))

(define input2
  (call-with-input-file "./input2" (lambda (port)
				       (read port))))



;; ------------ puzzle ---------------------------
;;(define p (solve "mmsxrhfx"))
;; "RLDUDRDDRR"

(define x 0)
(define y 0)

(define (reset)
  (set! x 0)
  (set! y 0))

(define (forward n)
  (format #t "moving forward ~a ~%" n)
  (set! x (+ x n))
  #t)

(define (down n)
  (format #t "moving down ~a ~%" n)
  (set! y (- y n)))

(define (up n)
  (format #t "moving up ~a ~%" n)
  (set! y (+ y n)))

(define (solve2 xs)
  (cond
   ((null? xs) #t)
   (#t (match (car xs)
	 (('forward x) (forward x))
	 (('down x) (down x))
	 (('up x) (up x))
	 (_ (error "solve2." (list "unknown .."))))
       (solve (cdr xs)))))

(define (solve in)
  (solve2 in))
 

;; ------------ test cases -------------
(define (test-1)
  (reset)
  (solve input)
  (* x y))


(define (test-2)
  (reset)
  (solve input2)
  (* x y))





#|
    If your passcode were ihgpwlah, the shortest path would be DDRRRD.
    With kglvqrro, the shortest path would be DDUDRLRRUDRD.
    With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.
|#
;; (assert (string=? (solve "ihgpwlah") "DDRRRD"))
;; (assert (string=? (solve "kglvqrro") "DDUDRLRRUDRD"))
;; (assert (string=? (solve "ulqzkmiv") "DRURDRUDDLLDLUURRDULRLDUUDDDRR"))




		    
		    
		    
		      
		    	 
		    
