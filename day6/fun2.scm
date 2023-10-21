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

(define input
  (call-with-input-file "./input" (lambda (port)
				       (read port))))


(define input2
  (call-with-input-file "./input2" (lambda (port)
				       (read port))))



;; ------------ puzzle ---------------------------
#|
list of integers
map over them decrease by one
if value given is zero - we replace with 6 and remember create new lantern fish with value 8

sequence of integers 3 4 3 1 2
reduce each one by one  2 3 2 0 1
reducing a zero results in flip to 6 and additional 8 being tacked on end
as more steps routine runs has to make n + m passes
results in a dramatic slow down

if instead we simply note how many 3's , 4's , 1's  , 2's we have
we should then be able to say in next generation all the 3's become 2's
all 0's become 6's and number of 8's becomes the number of 0's

8's become 7's
7's becomes 6's

|#
(define (1+ x) (+ x 1))

#|
take a list of integers in range 0 to 8 inclusive , return a vector of count each integer
|#
(define (make-v xs)
  (let* ((vlen 9)
	 (vfill 0)
	 (v (make-vector vlen vfill)))
    (do-list p xs
	     (assert (and (>= p 0) (<= p 8)))
	     (vector-set! v p (1+ (vector-ref v p))))
    (assert (= vlen (vector-length v)))
    v))



#|
given a vector
record number of zero elements
shift all values down by one from 1 to 8 inclusive -> 0 to 7 domain
set 8th entry to number of ones
set 6th entry to number of ones also
|#
(define (roll-down v)
  (let ((range-1to8 (cdr (iota 9))))
    (do-list n range-1to8
	     (vector-set! v (- n 1) (vector-ref v n))
	     (vector-set! v n 0))))

(define (stage v)
  (let ((n0 (vector-ref v 0)))
    (roll-down v)
    (vector-set! v 8 n0)
    (vector-set! v 6 (+ n0 (vector-ref v 6)))
    v))


(define *lantern-show* (make-parameter #f))

(define (lantern v end)
  (let ((res '()))
    (do-list step (iota (+ 1 end))
	     (set! res (cons (vector-copy v) res)) 
	     (cond
	      ((= step end)
	       (format #t "~a : ~a ........ ~a~%" step v (*lantern-show*)))	       
	      (#t (if (*lantern-show*)
		      (format #t "~a : ~a ........ ~a~%" step v (*lantern-show*))		      
		      (format #t "~a ~%" step))))
	     (stage v))
    (reverse res)))


;; ------------ unit test -------------
(define (compare)
  '(
   ( 3 4 3 1 2)
   ( 2 3 2 0 1)
   ( 1 2 1 6 0 8)
   ( 0 1 0 5 6 7 8)
   ( 6 0 6 4 5 6 7 8 8)
   ( 5 6 5 3 4 5 6 7 7 8)
   ( 4 5 4 2 3 4 5 6 6 7)
   ( 3 4 3 1 2 3 4 5 5 6)
   ( 2 3 2 0 1 2 3 4 4 5)
   ( 1 2 1 6 0 1 2 3 3 4 8)
   ( 0 1 0 5 6 0 1 2 2 3 7 8)
   ( 6 0 6 4 5 6 0 1 1 2 6 7 8 8 8)
   ( 5 6 5 3 4 5 6 0 0 1 5 6 7 7 7 8 8)
   ( 4 5 4 2 3 4 5 6 6 0 4 5 6 6 6 7 7 8 8)
   ( 3 4 3 1 2 3 4 5 5 6 3 4 5 5 5 6 6 7 7 8)
   ( 2 3 2 0 1 2 3 4 4 5 2 3 4 4 4 5 5 6 6 7)
   ( 1 2 1 6 0 1 2 3 3 4 1 2 3 3 3 4 4 5 5 6 8)
   ( 0 1 0 5 6 0 1 2 2 3 0 1 2 2 2 3 3 4 4 5 7 8)
   ( 6 0 6 4 5 6 0 1 1 2 6 0 1 1 1 2 2 3 3 4 6 7 8 8 8 8)
   ))

;; ------------ test cases -------------
(define (example-1)
  (let ((n-steps 18))
    (parameterize ((*lantern-show* #t))
      (lantern (make-v input2) n-steps))))

(define (example-2)
  (let ((n-steps 18))
    (parameterize ((*lantern-show* #f))
      (lantern (make-v input2) n-steps))))

;; mistakenly thought needed 5934 steps , only need 80 steps 
(define (test-1)
  (let* ((n-steps 80)
	 (result (lantern (make-v input) n-steps))
	 (end (last result)))
    (values end (apply + (vector->list end)))))

 
(define (test-2)
  (let* ((n-steps 256)
	 (result (lantern (make-v input) n-steps))
	 (end (last result)))
    (values end (apply + (vector->list end)))))

#|

part-1 : chicken timing ,t
,t (test-1)
80 : #(17812 55824 26134 53504 45450 42762 74312 18374 45242) ........ #f
0.002s CPU time, 740/102 mutations (total/tracked), 0/12 GCs (major/minor), maximum live heap: 1.06 MiB
#(17812 55824 26134 53504 45450 42762 74312 18374 45242)
379414

part-2 : chicken timing ,t

,t (test-2)
256 : #(144083816932 176086817004 181819751218 196842519500 229805868268 222529360802 281410955052 118884319740 153545244780) ........ #f
0.016s CPU time, 2206/319 mutations (total/tracked), 0/31 GCs (major/minor), maximum live heap: 1.08 MiB
#(144083816932 176086817004 181819751218 196842519500 229805868268 222529360802 281410955052 118884319740 153545244780)
1705008653296
; 2 values

|#
      

