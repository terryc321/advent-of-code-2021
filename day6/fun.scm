;;(import (lsp-server))

(import scheme)
(import (chicken format))
(import (chicken sort))
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

|#
(define (1+ x) (+ x 1))


(define (stage xs)
  (define new '())
  (define (next-gen p)
    (cond
     ((= p 0) (set! new (cons 8 new))
              6)
     (#t (- p 1))))
  (append (map next-gen xs) new))


(define *lantern-show* (make-parameter #f))

(define (lantern xs end)
  (let ((res xs))
    (do-list step (iota (+ 1 end))
	     (cond
	      ((= step end)
	       (format #t "~a : ~a ........ ~a~%" step res (*lantern-show*)))
	      (#t (if (*lantern-show*)
		      (format #t "~a : ~a ........ ~a~%" step res (*lantern-show*))		      
		      (format #t "~a ~%" step))))
	     (set! res (stage res)))))

;; ------------ test cases -------------
(define (example-1)
  (let ((n-steps 18))
    (parameterize ((*lantern-show* #t))
      (lantern input2 n-steps))))

(define (example-2)
  (let ((n-steps 18))
    (parameterize ((*lantern-show* #f))
      (lantern input2 n-steps))))

(define (test-1)
  (let ((n-steps 5934))
    (lantern input n-steps)))


(define (test-2) #t)
	    
;; cant brute force this one...		    
;;(test-1)

		      
		    	 
		    
