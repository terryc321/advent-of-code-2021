

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...


(use-modules (rnrs)) ;; assert

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules (scheme base))

;; --------------------- macros --------------------------
(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------


(define *debug* #f)

(define input #f)
(define input2 #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))


(set! input (get-input "input"))
(set! input2 (get-input "input2"))

;; --------------------- macros --------------------------

;; house keeping check all strings are same length
(define (check-input in)
  (let ((slen #f))
    (dolist (x in)
	    (cond
	     ((not slen) (set! slen (string-length x)))
	     ((not (= slen (string-length x))) (error "different lengths" (list x slen)))
	     (#t #t)))
    slen))

(check-input input)
(check-input input2)

#|
gamma rate - most common bit in each position
episolon rate - negation the least common bit in each position
power consumption multiply these two rates together

|#

(define (foo in)  
  (let* ((slen (string-length (car in)))
	 (ct 0)
	 (vec0 (make-vector slen 0))
	 (vec1 (make-vector slen 0))
	 (xs0 '())
	 (xs1 '())
	 )	
    (dolist (str in)
	    (dolist (i (iota slen))
		    (let ((ch (string-ref str i)))
		      (cond
		       ((char=? ch #\0) (vector-set! vec0 i (+ 1 (vector-ref vec0 i))))
		       ((char=? ch #\1) (vector-set! vec1 i (+ 1 (vector-ref vec1 i))))		       
		       (#t (error "bad character" (list ch)))))))
    (assert (= slen (vector-length vec0)))
    (assert (= slen (vector-length vec1)))
    ;; sa - most common
    ;; sb - least common     
    (let* ((in-len (length in))
	   (sa (make-string slen #\0))
	   (sb (make-string slen #\0)))
      (dolist (i (iota slen))
	      (assert (= in-len (+ (vector-ref vec0 i) (vector-ref vec1 i))))
	      (let ((a (vector-ref vec0 i))
		    (b (vector-ref vec1 i)))
		(cond
		 ((> a b)
		  (string-set! sa i #\0)
		  (string-set! sb i #\1))
		 (#t (string-set! sa i #\1)
		     (string-set! sb i #\0)))))
      (let* ((na (string->number sa 2))
	     (nb (string->number sb 2))
	     (pwr (* na nb)))
	(list pwr na sa nb sb)))))



;; ---------------------------------------------------

#|
scheme@(guile-user)> (foo input)
$16 = (1025636 3827 "111011110011" 268 "000100001100")
scheme@(guile-user)> 


power factor 1025636

|#










	  
