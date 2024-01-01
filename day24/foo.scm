
;;(import (lsp-server))

(import scheme)
(import (chicken process-context))
(import (chicken io))
(import (chicken format))

(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken random))
(import (chicken time))
(import (chicken repl))
(import procedural-macros)
(import regex)

;;(import srfi-89)


;;(import srfi-89)
;; missing srfi-89 compatibility no egg for it ??

;;(define pp pretty-print)

;;(import (chicken doc))
;; documentation

;; debugging macro expander
;; debugger



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

#|
(current-directory)
(change-directory "day24")
|#

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define (div x y) (floor (/ x y)))
(define (mod x y) (modulo x y))
(define (mul x y) (* x y))
(define (eql x y) (if (= x y) 1 0))
(define (add x y) (+ x y))

;; ====================== thinking of hash tables to store required z values ==================

(define *solutions* #f)

(define *solutions1* #f)
(define *solutions2* #f)
(define *solutions3* #f)
(define *solutions4* #f)
(define *solutions5* #f)
(define *solutions6* #f)
(define *solutions7* #f)
(define *solutions8* #f)
(define *solutions9* #f)
(define *solutions10* #f)
(define *solutions11* #f)
(define *solutions12* #f)
(define *solutions13* #f)
(define *solutions14* #f)

(define *targets* #f)


(define (show-solutions ht)
  ;; insert hashtable magic here
  (format #t "solutions => ~%(")
  (hash-table-walk ht (lambda (k v)
			(format #t "~a " k)))
  (format #t ")~%")
  )



;; fast expects a vector
;; (time (fast (list->vector (num->ilist 12345678912345))))
;; (time (exec '((x 0)(y 0)(w 0)(z 0)) (num->ilist 12345678912345) input))
(define (fast-z vv)
  (when (not (= (vector-length vv) 14))
    (format #t "fast-z expects a vector of length 14 , only given ~a ~%given => [~a]" (vector-length vv) vv))
  
  (let ((x 0)
	(y 0)
	(w 0)
	(z 0)
	(n1 (vector-ref vv 0))
	(n2 (vector-ref vv 1))
	(n3 (vector-ref vv 2))
	(n4 (vector-ref vv 3))
	(n5 (vector-ref vv 4))
	(n6 (vector-ref vv 5))
	(n7 (vector-ref vv 6))
	(n8 (vector-ref vv 7))
	(n9 (vector-ref vv 8))
	(n10 (vector-ref vv 9))
	(n11 (vector-ref vv 10))
	(n12 (vector-ref vv 11))
	(n13 (vector-ref vv 12))
	(n14 (vector-ref vv 13))
	)
    
    (set! w n1)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 10))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 1))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n2)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 11))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 9))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n3)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 14))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 12))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n4)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 13))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 6))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n5)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -6))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 9))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n6)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -14))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 15))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n7)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 14))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 7))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n8)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 13))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 12))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n9)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -8))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 15))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n10)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -15))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 3))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n11)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 10))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 6))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n12)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -11))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 2))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n13)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -13))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 10))
    (set! y (mul y x))
    (set! z (add z y))
    (set! w n14)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -4))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 12))
    (set! y (mul y x))
    (set! z (add z y))
    z))



#|

where z-prime refers to previous z-prime

z-prime-4 :
(+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))

z-prime-5 :
if n4 = n5 then takealpha else beta
alpha: (= zout (floor (/ z-prime 26)))
beta : (= zout (+ n5 9 (* 26 (floor (/ z-prime 26)))))))

z-prime-6 :

(= zout (div z-prime 26))
(= zout (+ n6 15 (* 26 (div z-prime 26))))))

z-prime-7 :
(= zout z-prime)
(= zout (+ n7 7 (* 26 z-prime)))))

z-prime-8 :
(= zout z-prime)
(= zout (+ n8 12 (* 26 z-prime)))))
			   
z-prime-9 :
(= zout (div z-prime 26))
(= zout (+ n9 15 (* 26 (div z-prime 26))))))
			   
z-prime-10 :
(= zout (div z-prime 26))
(= zout (+ 3 n10 (* 26 (div z-prime 26))))))
			     
z-prime-11 :
(= zout (+ 6 n11 (* 26 z-prime)))

z-prime-12 :
(= zout (div z-prime 26))
(= zout (+ n12 2 (* 26 (div z-prime 26))))))
			     
z-prime-13 :
(= zout (floor (/ z-prime 26)))
(= zout (+ n13 10 (* 26 (floor (/ z-prime 26)))))))

z-prime-14 :
(= zout (floor (/ z-prime 26)))
(= zout (+ n14 12 (* 26 (floor (/ z-prime 26)))))))		    

---------------------------
is it possible to get a negative zout ??
Q - on test-11 determined cannot be alternative path ?? is this true ??

|#

;; =========== code generator ===========================
(define (make-ftest n)
  (let ((fname (string->symbol (format #f "ftest~a" n)))
	(nn (string->symbol (format #f "n~a" n))))
  `(define (,fname z ,nn)
     (let* ((w 0)(x 0)(y 0)(win w)(xin x)(yin y)(zin z))
       ,@(comp n)
       (let* ((wout w)(xout x)(yout y)(zout z))
	 z)))))


(define (make-ftest-all)
  (do-list (n (cdr (iota 15)))
	   (pp (make-ftest n))))


;; ============== auto gne code =============================
(define (ftest1 z n1)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n1)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 10))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 1))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z))
      (assert (= z (+ n1 1)))
      z)))


#|

ftest1 : z=0 fixed   n1 1 .. 9      zout = 2 .. 10 
*solutions2*  ()



|#
(define (solutions1)
  (set! *solutions1* (make-hash-table))
  (let ((zin 0)
	(count 0)
	(computed 0))
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest1 zin n)))
		 (set! computed (+ 1 computed))
		 (let ((lookup (hash-table-ref/default *solutions1* zout #f)))
		   (cond
		    (lookup
		     (hash-table-set! *solutions1* zin (cons (list zin n) lookup))
		     #t)
		    (#t
		     (hash-table-set! *solutions1* zin (list (list zin n)))
		     #t)))))		     		  
      (format #t "computed ~a ~%" computed)))


(define (solutions2)
  (set! *solutions2* (make-hash-table))
  (let ((count 0)
	(computed 0))
    (hash-table-walk *solutions1* (lambda (k v)
				    (let ((zin k))
				      (do-list (n '(1 2 3 4 5 6 7 8 9))
					       (let ((zout (ftest2 zin n)))
						 (set! computed (+ 1 computed))
						 (let ((lookup (hash-table-ref/default *solutions2* zout #f)))
						   (cond
						    (lookup
						     (hash-table-set! *solutions2* zin (cons (list zin n) lookup))
						     #t)
						    (#t
						     (hash-table-set! *solutions2* zin (list (list zin n)))
						     #t))))))))		     		  
    (format #t "computed ~a ~%" computed)))

    


      







    


#|
ftest1 simply increments n1
ftest1 z input is a red herring as initial value always zero

ftest2 z n2
is rather (ftest2 (n1+1) n2)

n1 = 1 .. 9
then
z1 = 2 .. 10
n2 = 1 .. 9

|#


;; had introduce n1 in terms of z
;; zin is 2 .. 10 inclusive
(define (ftest2 z n2)
  (let* ((n1 (- z 1)) (w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n2) ;;
    (set! x (mul x 0)) ;; x = 0
    (set! x (add x z)) ;; x = 0 + z
    (set! x (mod x 26)) ;; x = mod z 26
    (set! z (div z 1))  ;; z = div z 1   ,,, nop 
    (set! x (add x 11))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 9))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z))
      (assert (= zout (+ n2 (* 26 n1) 35)))
      zout)))


(define ftest2-out 
  (let ((res '()))
    (do-list (zin (reverse '(2 3 4 5 6 7 8 9 10)))
	     (do-list (n2 (reverse '(1 2 3 4 5 6 7 8 9)))
		      (let* ((zout (ftest2 zin n2))
			     (n1 (- zin 1))
			     (p (list 'zout zout 'zin zin 'prop (= zout (+ n2 (* 26 n1) 35)) 'n1 n1 'n2 n2)))
			(set! res (cons p res)))))
    (reverse res)))

(define (solutions2)
  (set! *solutions2* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest2 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions3* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions2* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))










(define (ftest3 z n3)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n3)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 14))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 12))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z))
      zout)))


(define (solutions3)
  (set! *solutions3* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest3 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions4* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions3* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))




(define (ftest4 z n4)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n4)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 13))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 6))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z))
      zout)))




(define (solutions4)
  (set! *solutions4* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest4 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions5* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions4* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))



(define (ftest5 z n5)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n5)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -6))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 9))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z))
      zout)))

(define (solutions5)
  (set! *solutions5* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest5 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions6* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions5* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))




(define (ftest6 z n6)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n6)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -14))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 15))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))



(define (solutions6)
  (set! *solutions6* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest6 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions7* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions6* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))




(define (ftest7 z n7)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n7)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 14))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 7))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))

(define (solutions7)
  (set! *solutions7* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest7 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions8* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions7* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))



(define (ftest8 z n8)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n8)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 13))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 12))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))


;;; 68057 ..... killed as keeps going indefinitely ------- nope wrong ftest running ftest9 instead ftest8
(define (solutions8)
  (set! *solutions8* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest8 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions9* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions8* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))




(define (ftest9 z n9)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n9)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -8))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 15))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))

;;; 33201 solutions ...??
(define (solutions9)
  (set! *solutions9* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest9 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions10* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions9* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))






(define (ftest10 z n10)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n10)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -15))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 3))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))


(define (solutions10)
  (set! *solutions10* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest10 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions11* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions10* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))






(define (ftest11 z n11)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n11)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 1))
    (set! x (add x 10))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 6))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))


(define (solutions11)
  (set! *solutions11* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest11 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions12* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions11* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))




#|


|#

(define (ftest12 z n12)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n12)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -11))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 2))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))


(define (solutions12)
  (set! *solutions12* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest12 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions13* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions12* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))




#|
 can we get ftest13 to output [31 .. 39] ??

|#
(define (ftest13 z n13)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n13)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26))
    (set! z (div z 26))
    (set! x (add x -13))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 10))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z)) z)))



(define (solutions13)
  (set! *solutions13* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest13 zin n)))
		 (cond     ;; look at solutions found from (solutions13) if any match input required
		  ((hash-table-ref/default *solutions14* zout #f)
		   (set! count (+ 1 count))
		   (hash-table-set! *solutions13* zin #t)
		   (format #t "solution no [~a] :  zin ~a : n ~a : zout ~a ~%" count zin n zout)))))
      ;; outside trying n13 ... 1... 9 inclusive
      (set! zin (+ zin 1)))))



#|


|#

#|

suppose do not know z in ftest14
lets fix n14 as 1
how characterise z ?


|#
;; ftest14 solved with  n14 = [1..9] z = n14 + 30
;; zin needs to be [31 to 39] inclusive
(define (ftest14 z n14)
  (let* ((w 0) (x 0) (y 0) (win w) (xin x) (yin y) (zin z))
    (set! w n14)
    (set! x (mul x 0))
    (set! x (add x z))
    (set! x (mod x 26)) ;; (mod z 26)  = n14 + 4         n14 { 1..9}  {5 .. 13} = z mod 26
    (set! z (div z 26))
    (set! x (add x -4))
    (set! x (eql x w))
    (set! x (eql x 0))
    (set! y (mul y 0))
    (set! y (add y 25))
    (set! y (mul y x))
    (set! y (add y 1))
    (set! z (mul z y))
    (set! y (mul y 0))
    (set! y (add y w))
    (set! y (add y 12))
    (set! y (mul y x))
    (set! z (add z y))
    (let* ((wout w) (xout x) (yout y) (zout z))
      (or (assert (or (= zout (div zin 26))
		      (= zout (+ n14 12 (* 26 (div zin 26))))))
	  (error "missing something!"))
      zout)))



(define (solutions14)
  (set! *solutions14* (make-hash-table))
  (let ((zin 0)
	(count 0))
    (do-while #t
	      (do-list (n '(1 2 3 4 5 6 7 8 9))
		       (let ((zout (ftest14 zin n)))
			 (cond    
			  ((= zout 1) 
			   (set! count (+ 1 count))
			   (hash-table-set! *solutions14* zin #t)
			   (format #t "solutions-14 no [~a] :  zin ~a : n ~a : zout ~a ~%" count zin n zout)))))
	      ;; outside trying n13 ... 1... 9 inclusive
	      (set! zin (+ zin 1)))))








;; ============= end of auto gen code ==================================

(define (seek fun fok)
  (do-list (n (reverse (cdr (iota 10))))
	   (do-for zin (0 500000)
		   (let ((zout (fun zin n)))
		     ;;(format #t "~a : ~a : ~a ~%" n zin zout)
		     (cond
		      ((fok zin n zout)
		       (format #t "zin [~a] : n [~a] : zout[~a] ~%" zin n zout)
		       (format #t "******* solution !!~%")))))))



;; ftest13 needs to give out 31 .. 39 
;; (define (solve13)
;;   (seek ftest13 (lambda (zin n zout) (if (and (>= zout 31) (<= zout 39)) #t #f))))

#|
(define (solve12 ztarget)
  (seek ftest12 (lambda (zin n zout) (if (= zout ztarget) (begin (solve12 zin) #t) #f))))

(define (solve13 ztarget)
  (seek ftest13 (lambda (zin n zout) (if (= zout ztarget) (begin (solve12 zin) #t) #f))))

(define (solve14)
  (seek ftest14 (lambda (zin n zout) (if (= zout 1) (begin (solve13 zin) #t) #f))))

|#

;; ((lambda (x) (if (= x 1) #t #f)) 1)
;; ((lambda (x) (if (= x 1) #t #f)) 2)
;; solve ftest14 using (seek ftest14)

;; ============================== code generator ===========================================

(define (make-solve n)
  (let ((nn (string->symbol (format #f "n~a" n)))	
	(fname1 (string->symbol (format #f "solve~a" n)))
	(fname2 (string->symbol (format #f "ftest~a" n)))
	(fname3 (string->symbol (format #f "solve~a" (- n 1)))))
    `(define (,fname1 ztarget)
       (seek ,fname2 (lambda (zin n zout) (if (= zout ztarget) (begin
								 (set! ,nn n)
								 #|
								 (format #t ,(string-append (format #f "~a => " nn)
											    (format #f "~a~a" #\~ #\a)
											    (format #f "~a~a" #\~ #\%)
											    )
								 n)
								 |#
								 (,fname3 zin)
								 #t) #f))))))



(define (make-solve-all)
  (do-list (n (cdr (iota 15)))
	   (pp (make-solve n))))
       

;; =========================== gen code ===================================================

(define (start)
  (let ((n1 0)
	(n2 0)
	(n3 0)
	(n4 0)
	(n5 0)
	(n6 0)
	(n7 0)
	(n8 0)
	(n9 0)
	(n10 0)
	(n11 0)
	(n12 0)
	(n13 0)
	(n14 0))  
   
    
;; dummy 
(define (solve0 zin)  
  (when
      (= zin 0)
    (format #t "solution ~a ~%" (list n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14))
    ;; check
    ))



(define (solve1 ztarget)
  (seek ftest1
        (lambda (zin n zout)
          (if (= zout ztarget)
              (begin (set! n1 n)
		     (format #t "n1 => ~a~%" n)
		     (solve0 zin) #t)
            #f))))

(define (solve2 ztarget)
  (seek ftest2
        (lambda (zin n zout)
          (if (= zout ztarget)
              (begin (set! n2 n)
		     (format #t "n2 => ~a~%" n)
		     (solve1 zin) #t)
            #f))))

(define (solve3 ztarget)
  (seek ftest3
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n3 n) (format #t "n3 => ~a~%" n) (solve2 zin) #t)
            #f))))

(define (solve4 ztarget)
  (seek ftest4
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n4 n) (format #t "n4 => ~a~%" n) (solve3 zin) #t)
            #f))))

(define (solve5 ztarget)
  (seek ftest5
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n5 n) (format #t "n5 => ~a~%" n) (solve4 zin) #t)
            #f))))

(define (solve6 ztarget)
  (seek ftest6
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n6 n) (format #t "n6 => ~a~%" n) (solve5 zin) #t)
            #f))))

(define (solve7 ztarget)
  (seek ftest7
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n7 n) (format #t "n7 => ~a~%" n) (solve6 zin) #t)
            #f))))

(define (solve8 ztarget)
  (seek ftest8
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n8 n) (format #t "n8 => ~a~%" n) (solve7 zin) #t)
            #f))))

(define (solve9 ztarget)
  (seek ftest9
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n9 n) (format #t "n9 => ~a~%" n) (solve8 zin) #t)
            #f))))

(define (solve10 ztarget)
  (seek ftest10
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n10 n) (format #t "n10 => ~a~%" n) (solve9 zin) #t)
            #f))))

(define (solve11 ztarget)
  (seek ftest11
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n11 n) (format #t "n11 => ~a~%" n) (solve10 zin) #t)
            #f))))

(define (solve12 ztarget)
  (seek ftest12
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n12 n) (format #t "n12 => ~a~%" n) (solve11 zin) #t)
            #f))))

(define (solve13 ztarget)
  (seek ftest13
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n13 n) (format #t "n13 => ~a~%" n) (solve12 zin) #t)
            #f))))

(define (solve14 ztarget)
  (seek ftest14
        (lambda (zin n zout)
          (if (= zout ztarget)
            (begin (set! n14 n) (format #t "n14 => ~a~%" n) (solve13 zin) #t)
            #f))))


;; want a 1 out on n14
  (solve14 1)
  )) ;; start


;;(start)





		       


#|
        w x y z n
(ftest1 0 0 0 0 n1)    => n1+1
(ftest2 0 0 0 n1+1 n2)  

no point passing in w x y z
w set to relevant n1 n2 ...
x , y reset
z is output from previous ftest


(ftest1 z n1)
(ftest1 0 1) => 2  outputs n1+1
..
(ftest 0 9) => 10 

|#

;; ================ code generator ==================================

(define (make-gtest b)
  (let ((res '()))
    (do-list (n (cdr (iota (+ b 1))))
	     (cond
	      ((= n 1) (let ((z 0)
			     (fname (string->symbol (format #f "ftest~a" n)))
			     (nn (string->symbol (format #f "n~a" n))))
			 (set! res `(,fname ,z ,nn))))
	      (#t (let ((fname (string->symbol (format #f "ftest~a" n)))
			(nn (string->symbol (format #f "n~a" n))))
		    (set! res `(,fname ,res ,nn))))))
    (let ((n b))
      (let ((fname (string->symbol (format #f "gtest~a" n))))
	(let ((args '()))
	  (do-for i (1 (+ n 1))
		  (let ((arg-n (string->symbol (format #f "n~a" i))))
		    (set! args (cons arg-n args))))	
	  `(define (,fname ,@(reverse args))
	     ,res))))))

(define (make-gtest-all)
  (do-list (n (cdr (iota 15)))
	   (pp (make-gtest n))))

;;; ============= auto generated code =======================

(define (gtest1 n1) (ftest1 0 n1))
(define (gtest2 n1 n2) (ftest2 (ftest1 0 n1) n2))
(define (gtest3 n1 n2 n3) (ftest3 (ftest2 (ftest1 0 n1) n2) n3))
(define (gtest4 n1 n2 n3 n4) (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4))
(define (gtest5 n1 n2 n3 n4 n5)
  (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5))
(define (gtest6 n1 n2 n3 n4 n5 n6)
  (ftest6 (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5) n6))
(define (gtest7 n1 n2 n3 n4 n5 n6 n7)
  (ftest7
    (ftest6 (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5) n6)
    n7))
(define (gtest8 n1 n2 n3 n4 n5 n6 n7 n8)
  (ftest8
    (ftest7
      (ftest6 (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5) n6)
      n7)
    n8))
(define (gtest9 n1 n2 n3 n4 n5 n6 n7 n8 n9)
  (ftest9
    (ftest8
      (ftest7
        (ftest6
          (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5)
          n6)
        n7)
      n8)
    n9))
(define (gtest10 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10)
  (ftest10
    (ftest9
      (ftest8
        (ftest7
          (ftest6
            (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5)
            n6)
          n7)
        n8)
      n9)
    n10))
(define (gtest11 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11)
  (ftest11
    (ftest10
      (ftest9
        (ftest8
          (ftest7
            (ftest6
              (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5)
              n6)
            n7)
          n8)
        n9)
      n10)
    n11))
(define (gtest12 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12)
  (ftest12
    (ftest11
      (ftest10
        (ftest9
          (ftest8
            (ftest7
              (ftest6
                (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5)
                n6)
              n7)
            n8)
          n9)
        n10)
      n11)
    n12))
(define (gtest13 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13)
  (ftest13
    (ftest12
      (ftest11
        (ftest10
          (ftest9
            (ftest8
              (ftest7
                (ftest6
                  (ftest5 (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4) n5)
                  n6)
                n7)
              n8)
            n9)
          n10)
        n11)
      n12)
    n13))
(define (gtest14 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14)
  (ftest14
    (ftest13
      (ftest12
        (ftest11
          (ftest10
            (ftest9
              (ftest8
                (ftest7
                  (ftest6
                    (ftest5
                      (ftest4 (ftest3 (ftest2 (ftest1 0 n1) n2) n3) n4)
                      n5)
                    n6)
                  n7)
                n8)
              n9)
            n10)
          n11)
        n12)
      n13)
    n14))
;;====================== auto gen code above ==========================

;; ================= code generator =============================
#|
build the s expression out wards

|#
(define (make-brute)
  (let* ((core '(let ((s (gtest14 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14)))
		  (when (= s 1)
		    (format #t "solution found ~%  ~a  ~%" (list n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14)))))
	 (res core))
    (do-list (n (reverse (cdr (iota 10))))
	     (let ((nn (string->symbol (format #f "n~a" n))))
	       (set! res `(do-list (,nn vals) ,res))))
    `(define (brute)
       (let ((vals ',(cdr (iota 15))))
	 ,res))))

;;======================= auto gen code ============================================
(define (brute)
  (let ((vals (reverse '(1 2 3 4 5 6 7 8 9)))
	(inc 0)
	(limit 1000000)
	(tick (current-seconds))
	(tock (current-seconds)))
    (do-list
      (n1 vals)
      (do-list
        (n2 vals)
        (do-list
          (n3 vals)
          (do-list
            (n4 vals)
            (do-list
              (n5 vals)
              (do-list
                (n6 vals)
                (do-list
                  (n7 vals)
                  (do-list
                    (n8 vals)
                    (do-list
                      (n9 vals)
                      (do-list
                        (n10 vals)
                        (do-list
                          (n11 vals)
                          (do-list
                            (n12 vals)
                            (do-list
                              (n13 vals)
                              (do-list
                                (n14 vals)
                                (let ((s (gtest14
                                           n1
                                           n2
                                           n3
                                           n4
                                           n5
                                           n6
                                           n7
                                           n8
                                           n9
                                           n10
                                           n11
                                           n12
                                           n13
                                           n14)))
				  (set! tock (current-seconds))
				  (set! inc (+ inc 1))
				  (when (> inc limit)
				    (set! inc 0)
				    (format #t "time taken ~a seconds for counter limit ~a ~%"
					    (- tock tick)
					    limit)
				    (set! tick (current-seconds))
				    (format
                                     #t
                                     "trying  ~a ==> ~a ~%"
                                     (list n1
                                           n2
                                           n3
                                           n4
                                           n5
                                           n6
                                           n7
                                           n8
                                           n9
                                           n10
                                           n11
                                           n12
                                           n13
                                           n14)
				     s)
				    (set! tick (current-seconds)))
                                  (when (= s 1)
                                        (format
                                          #t
                                          "solution found ~%  ~a  ~%"
                                          (list n1
                                                n2
                                                n3
                                                n4
                                                n5
                                                n6
                                                n7
                                                n8
                                                n9
                                                n10
                                                n11
                                                n12
                                                n13
                                                n14))))))))))))))))))))

;;===============================================================================
(define (help)
  (do-list (n '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
	   (format #t "(n~a (+ 1 (pseudo-random-integer 9)))~%" n)))



;;===============================================================================

(define (brute2)
  (do-while #t	    
	    (let ((n1 (+ 1 (pseudo-random-integer 9)))
		  (n2 (+ 1 (pseudo-random-integer 9)))
		  (n3 (+ 1 (pseudo-random-integer 9)))
		  (n4 (+ 1 (pseudo-random-integer 9)))
		  (n5 (+ 1 (pseudo-random-integer 9)))
		  (n6 (+ 1 (pseudo-random-integer 9)))
		  (n7 (+ 1 (pseudo-random-integer 9)))
		  (n8 (+ 1 (pseudo-random-integer 9)))
		  (n9 (+ 1 (pseudo-random-integer 9)))
		  (n10 (+ 1 (pseudo-random-integer 9)))
		  (n11 (+ 1 (pseudo-random-integer 9)))
		  (n12 (+ 1 (pseudo-random-integer 9)))
		  (n13 (+ 1 (pseudo-random-integer 9)))
		  (n14 (+ 1 (pseudo-random-integer 9))))
              (let ((s (gtest14
                        n1
                        n2
                        n3
                        n4
                        n5
                        n6
                        n7
                        n8
                        n9
                        n10
                        n11
                        n12
                        n13
                        n14)))
                (when (= s 1)
                  (format
                   #t
                   "solution found ~%  ~a  ~%"
                   (list n1
                         n2
                         n3
                         n4
                         n5
                         n6
                         n7
                         n8
                         n9
                         n10
                         n11
                         n12
                         n13
                         n14)))))))


;;(brute2)



;; =========================== code generator ==========================
(define (make-solution n)
  (let ((fname1 (string->symbol (format #f "solution-forward~a" n)))
	(hash-name1 (string->symbol (format #f "*solutions~a*" n)))
	(hash-name2 (string->symbol (format #f "*solutions~a*" (- n 1))))
	(ftest-name (string->symbol (format #f "ftest~a" n)))	
	(nn (string->symbol (format #f "n~a" n))))
    `(define (,fname1)
       (set! ,hash-name1 (make-hash-table))
       (let ((count 0))
	 (hash-table-walk ,hash-name2 (lambda (k v)
					 (let ((zin k))
					   (do-list (n '(1 2 3 4 5 6 7 8 9))
						    (let ((zout (,ftest-name zin n)))
						      (let ((val (hash-table-ref/default ,hash-name1 zout #f)))
							(cond
							 (val #t)
							 (#t (hash-table-set! ,hash-name1 zout #t)
							     (set! count (+ count 1))
							     ;;(format #t "solution no [~a] :  zin ~a : n ~a : zout ~a ~%" count zin n zout)
							     
							     ))))))))
	 count))))





(define (make-solution-all)
  (do-list (n (cdr (cdr (iota 15))))
	   (pp (make-solution n))))


;; ======================== hand coded solution-forward1 generated code ==================================

(define (solution-forward1)
  (set! *solutions1* (make-hash-table))
  (let ((zin 0)
	(count 0))
      (do-list (n '(1 2 3 4 5 6 7 8 9))
	       (let ((zout (ftest1 zin n)))
		 (let ((val (hash-table-ref/default *solutions1* zout #f)))
		   (cond
		    (val #t)
		    (#t (hash-table-set! *solutions1* zout #t)
			(set! count (+ count 1))
			;;(format #t "solution no [~a] :  zin ~a : n13 ~a : zout ~a ~%" count zin n zout)
			)))))
      count))



;; ======================= generated code ===================================

(define (solution-forward2)
  (set! *solutions2* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions1*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest2 zin n)))
              (let ((val (hash-table-ref/default *solutions2* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions2* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward3)
  (set! *solutions3* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions2*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest3 zin n)))
              (let ((val (hash-table-ref/default *solutions3* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions3* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward4)
  (set! *solutions4* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions3*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest4 zin n)))
              (let ((val (hash-table-ref/default *solutions4* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions4* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward5)
  (set! *solutions5* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions4*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest5 zin n)))
              (let ((val (hash-table-ref/default *solutions5* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions5* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward6)
  (set! *solutions6* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions5*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest6 zin n)))
              (let ((val (hash-table-ref/default *solutions6* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions6* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward7)
  (set! *solutions7* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions6*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest7 zin n)))
              (let ((val (hash-table-ref/default *solutions7* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions7* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward8)
  (set! *solutions8* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions7*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest8 zin n)))
              (let ((val (hash-table-ref/default *solutions8* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions8* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward9)
  (set! *solutions9* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions8*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest9 zin n)))
              (let ((val (hash-table-ref/default *solutions9* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions9* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward10)
  (set! *solutions10* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions9*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest10 zin n)))
              (let ((val (hash-table-ref/default *solutions10* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions10* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward11)
  (set! *solutions11* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions10*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest11 zin n)))
              (let ((val (hash-table-ref/default *solutions11* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions11* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward12)
  (set! *solutions12* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions11*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest12 zin n)))
              (let ((val (hash-table-ref/default *solutions12* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions12* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward13)
  (set! *solutions13* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions12*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest13 zin n)))
              (let ((val (hash-table-ref/default *solutions13* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions13* zout #t)
                       (set! count (+ count 1))))))))))
    count))
(define (solution-forward14)
  (set! *solutions14* (make-hash-table))
  (let ((count 0))
    (hash-table-walk
      *solutions13*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest14 zin n)))
              (let ((val (hash-table-ref/default *solutions14* zout #f)))
                (cond (val #t)
                      (#t
                       (hash-table-set! *solutions14* zout #t)
                       (set! count (+ count 1))))))))))
    count))

#|
(do-list (n '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
	 (let ((name (string->symbol (format #f "solution-forward~a" n)))
	       (message (format #f "solutions ... ~a ....... " n)))
	   (format #t "~a~%" `(format #t "~a ~a~%" message ,n (,name)))))
|#

(define (run)

(time (format #t "~a ~a : ~a~%" "solutions ... " 1 (solution-forward1)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 2 (solution-forward2)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 3 (solution-forward3)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 4 (solution-forward4)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 5 (solution-forward5)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 6 (solution-forward6)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 7 (solution-forward7)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 8 (solution-forward8)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 9 (solution-forward9)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 10 (solution-forward10)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 11 (solution-forward11)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 12 (solution-forward12)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 13 (solution-forward13)))
(time (format #t "~a ~a : ~a~%" "solutions ... " 14 (solution-forward14)))


;; BUG !! was looking for z = 1 as output , but that was wRONG ! supposed to be z = 0 output !!!
(let ((n (hash-table-ref/default *solutions14* 0 #f)))
  (cond
   (n (format #t "good news ....... there is a solution resulting in zout = 0 output~%"))
   (#t (format #t "bad news ......... there is no solution resulting in zout = 0 output~%"))))

)



#|
hash tables simply record the output values zout of various functions

of all values reaching solutions14 how may result in a 1 ??

;; *pruning* - here suppose existence of

idea of pruning

prune13 
prune *solutions13* such that ftest14 produces zero
*solutions13* stores output zout ftest13 - zin ftest14
if zout ftest14 is not zero , for any n14 tried then that entry can be removed from *solutions13* hash table

prune12
prune *solutions12* such that ftest13 produces a value in *solutions13*
again any number in *solutions12 that does not result in a usable value for solutions13 can be removed



|#
;; ================================ pruning =====================================
(define (make-prune n)
  (let ((fname1 (string->symbol (format #f "prune~a" n)))
	(hash-name1 (string->symbol (format #f "*solutions~a*" n)))
	(hash-name2 (string->symbol (format #f "*solutions~a*" (+ n 1))))
	(ftest-name (string->symbol (format #f "ftest~a" (+ n 1))))	
	(nn (string->symbol (format #f "n~a" n))))
    `(define (,fname1)
       (let ((count 0)
	     (keep-count 0)
	     (remove '())
	     (remove-count 0))
	 (format #t "pruning hash table ~a ~%" ,n)
	 (hash-table-walk
	  ,hash-name1
	  (lambda (k v)
            (let ((zin k))
              (do-list
               (n '(1 2 3 4 5 6 7 8 9))
               (let ((zout (,ftest-name zin n)))
		 (let ((val (hash-table-ref/default ,hash-name2 zout #f)))
		   (cond
		    (val 
		     (set! keep-count (1+ keep-count))
		     #t)
		    (#t (set! remove (cons k remove))
			(set! remove-count (1+ remove-count))
			))))))))
	 (format #t "can remove ~a undesirables ~%" remove-count)
	 (do-list (r remove)
		  (hash-table-delete! ,hash-name1 r))
	 (set! remove #f)
	 (format #t "we keep ~a ~%" keep-count)))))


(define (make-all-prune)
  (do-list (n (reverse (cdr (iota 13))))
	   (pp (make-prune n))
	   (newline)))

(define (make-all-prune-calls)
  (do-list (n (reverse (cdr (iota 13))))
	   (format #t "(prune ~a)~%" n)))




;; ====================================================================================

(define (prune13)
  (let ((count 0)
	(keep-count 0)
	(remove '())
	(remove-count 0))
    (format #t "pruning hash table 13 ~%")
    (hash-table-walk
      *solutions13*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest14 zin n)))
	      (cond
	       ((= zout 0)
		(set! keep-count (1+ keep-count))
		#t)
	       (#t (set! remove (cons zin remove))
		   (set! remove-count (1+ remove-count))
		   )))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove)
	     (hash-table-delete! *solutions13* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))


;; ========================== generic pruning .................

(define (prune12)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 12)
    (hash-table-walk
      *solutions12*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest13 zin n)))
              (let ((val (hash-table-ref/default *solutions13* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions12* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune11)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 11)
    (hash-table-walk
      *solutions11*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest12 zin n)))
              (let ((val (hash-table-ref/default *solutions12* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions11* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune10)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 10)
    (hash-table-walk
      *solutions10*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest11 zin n)))
              (let ((val (hash-table-ref/default *solutions11* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions10* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune9)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 9)
    (hash-table-walk
      *solutions9*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest10 zin n)))
              (let ((val (hash-table-ref/default *solutions10* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions9* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune8)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 8)
    (hash-table-walk
      *solutions8*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest9 zin n)))
              (let ((val (hash-table-ref/default *solutions9* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions8* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune7)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 7)
    (hash-table-walk
      *solutions7*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest8 zin n)))
              (let ((val (hash-table-ref/default *solutions8* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions7* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune6)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 6)
    (hash-table-walk
      *solutions6*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest7 zin n)))
              (let ((val (hash-table-ref/default *solutions7* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions6* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune5)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 5)
    (hash-table-walk
      *solutions5*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest6 zin n)))
              (let ((val (hash-table-ref/default *solutions6* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions5* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune4)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 4)
    (hash-table-walk
      *solutions4*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest5 zin n)))
              (let ((val (hash-table-ref/default *solutions5* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions4* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune3)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 3)
    (hash-table-walk
      *solutions3*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest4 zin n)))
              (let ((val (hash-table-ref/default *solutions4* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions3* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune2)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 2)
    (hash-table-walk
      *solutions2*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest3 zin n)))
              (let ((val (hash-table-ref/default *solutions3* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions2* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))

(define (prune1)
  (let ((count 0) (keep-count 0) (remove '()) (remove-count 0))
    (format #t "pruning hash table ~a ~%" 1)
    (hash-table-walk
      *solutions1*
      (lambda (k v)
        (let ((zin k))
          (do-list
            (n '(1 2 3 4 5 6 7 8 9))
            (let ((zout (ftest2 zin n)))
              (let ((val (hash-table-ref/default *solutions2* zout #f)))
                (cond (val (set! keep-count (1+ keep-count)) #t)
                      (#t
                       (set! remove (cons k remove))
                       (set! remove-count (1+ remove-count))))))))))
    (format #t "can remove ~a undesirables ~%" remove-count)
    (do-list (r remove) (hash-table-delete! *solutions1* r))
    (set! remove #f)
    (format #t "we keep ~a ~%" keep-count)))



;; ==========================


(run)

#|
(prune13)
(prune12)
(prune11)
(prune10)
(prune9)
(prune8)
(prune7)
(prune6)
(prune5)
(prune4)
(prune3)
(prune2)
(prune1)

|#


#|

;; in the land of er-macro-transformer no first , no second , no third functions defined 
(define-syntax swap
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((x (car (cdr exp)))
	   (y (car (cdr (cdr exp))))
	   (tmp (gensym "tmp")))
       `(let ((,tmp ,x))
	  (set! ,x ,y)
	  (set! ,y ,tmp))))))

(define-macro (swap x y)
  (let ((tmp (gensym "tmp")))
    `(begin (let ((,tmp ,x))
	      (set! ,x ,y)
	      (set! ,y ,tmp)))))
|#



#|
             make temp hash table
for each in
solutions14 -----> zero ? 
table                                       YES ... copy val to new solutions13 hash
                                            NO ....  ignore it
wHEN complete
  solutions14  <=   temp hash table


|#

(define (for14)
  (let ((keep 0)
	(dump 0)
	(tot 0)
	(tot2 0)
	(ht (make-hash-table)))
    
    (hash-table-walk
     *solutions14*
     (lambda (k v)
       (let ((zout k))
	 (set! tot (+ tot 1))
	 (cond
	  ((= zout 0)
	   (set! keep (+ keep 1))
	   (hash-table-set! ht zout 0))
	  (#t
	   (set! dump (+ dump 1)))))))

    (set! *solutions14* ht)
    
    (format #t "originally there were ~a solutions for 14 ~%" tot2)
    (format #t "there are ~a kept / ~a dumped / ~a total ~%" keep dump tot)))


 
#|
             make temp hash table
for each in
solutions13 -----> ftest14 ----------->   entry in solutions14  ? 
table                                       YES ... copy val to new solutions13 hash
                                            NO ....  ignore it
wHEN complete
  solutions14  <=   temp hash table

there are 6 kept / 57774135 dumped / 57774141 total 
#;> (show-solutions *solutions14*)
solutions => 
(0 )
#;> (show-solutions *solutions13*)
solutions => 
(9 8 10 5 7 6 )
#;> 

|#


(define-macro (for-generic name ht2 ht3 fn3)
  `(define (,name)
     (let ((keep 0)
	   (dump 0)
	   (tot 0)
	   (tot2 0)
	   (ht (make-hash-table)))
       
       ;; take all outputs of 13 and feed them into 14 
       (hash-table-walk
	,ht2 ;;*solutions11*
	(lambda (k v)
	  (let* ((zin k))
	    (do-list
             (n '(1 2 3 4 5 6 7 8 9))
             (let ((zout (,fn3 zin n))) 
	       (set! tot (+ tot 1))
               (let ((val (hash-table-ref/default ,ht3 zout #f)))
                 (cond
		  (val
		   (set! keep (1+ keep))
		   (hash-table-set! ht zin #t))
                  (#t
		   (set! dump (1+ dump))))))))))
       
       (set! ,ht2 ht)      
       (format #t "there are ~a kept / ~a dumped / ~a total ~%" keep dump tot))))





(for-generic for13 *solutions13* *solutions14* ftest14)

#|
(for13)
there are 6 kept / 48 dumped / 54 total 
#;> (show-solutions *solutions13*)
solutions => 
(9 8 10 5 7 6 )
|#



#|
#;> (for12)
there are 30 kept / 58745706 dumped / 58745736 total 
#;> (show-solutions *solutions12*)
solutions => 
(148 147 146 145 144 174 173 172 171 170 223 222 278 277 276 199 275 198 274 197 196 200 252 251 250 249 248 226 225 224 )
#;> 
|#
(for-generic for12 *solutions12* *solutions13* ftest13)


#|

(for11)
there are 120 kept / 57736680 dumped / 57736800 total 
#;> (show-solutions *solutions11*)
solutions => 
(6512 3785 3784 5839 5838 5837 5836 3783 3782 6489 6488 6487 6486 5811
5810 5813 5812 3759 3758 3757 3756 6463 6462 6461 6460 5787 5786 5785
5784 5215 5214 4539 5213 4538 5212 4537 4536 5187 5186 5189 5188 4513
4512 4511 4510 5163 5162 5161 5160 5111 5110 5109 5108 4487 4486 4485
4484 5137 5136 5135 5134 7243 7242 7241 7240 3863 3862 3861 3860 6567
6566 6565 4459 6564 4458 5891 5890 5889 5888 7217 4461 7216 4460 7163
7162 7165 7164 7215 7214 6539 6538 6541 4435 6540 4434 4433 4432 7139
7138 7137 7136 7191 7190 7189 7188 3835 3834 3837 3836 5865 5864 3811
3810 3809 3808 5863 5862 6515 6514 6513 )

|#
(for-generic for11 *solutions11* *solutions12* ftest12)

#|
#;> (for10)
there are 120 kept / 6415080 dumped / 6415200 total 
#;> (show-solutions *solutions10*)
solutions => 
(274 275 278 276 277 222 223 198 199 196 197 200 250 251 248 249 252
226 224 225 146 147 144 145 148 170 171 174 172 173 )
|#
(for-generic for10 *solutions10* *solutions11* ftest11)

#|
#;> (for9)
there are 30 kept / 6157833 dumped / 6157863 total 
#;> (show-solutions *solutions9*)
solutions => 
(4514 5138 4540 5164 7244 5112 6542 6568 7192 4436 4462 7140 7218 3786 5892 3864 7166 5840 6464 5866 3812 6490 3838 5788 6516 5190 4488 3760 5216 5814 )
#;> 
|#
(for-generic for9 *solutions9* *solutions10* ftest10)

#|
#;> (for8)
there are 150 kept / 5550456 dumped / 5550606 total 
#;> (show-solutions *solutions8*)
solutions => 
(168080 168081 168756 168757 168753 168754 168755 132925 132926 132927
98452 98453 99128 99129 98449 98450 98451 115352 99125 115353 99126
99127 115349 115350 115351 170781 170782 170783 169432 169433 135629
135630 135631 169429 169430 169431 186332 186333 170784 170785 186329
186330 134280 186331 134281 187008 187009 134956 134957 134277 134278
134279 151180 134953 151181 134954 134955 151177 151178 151179 135632
185656 135633 185657 151853 151854 151855 185653 185654 99804 152532
185655 99805 133604 152533 133605 99801 152529 99802 133601 152530
99803 133602 152531 100480 133603 150504 100481 150505 116701 116702
150501 116703 150502 117380 150503 117381 117377 117378 117379 151856
151857 116028 116029 116025 116026 116027 116704 116705 97773 97774
97775 188360 188361 188357 188358 188359 170108 170109 97776 170105
97777 170106 170107 187005 187006 187007 187684 187685 187681 187682
187683 132928 132929 153208 153209 100477 153205 100478 153206 100479
153207 118056 118057 118053 118054 118055 168077 168078 168079 )

|#
(for-generic for8 *solutions8* *solutions9* ftest9)


#|
#;> (for7)
there are 150 kept / 616584 dumped / 616734 total 
#;> (show-solutions *solutions7*)
solutions => 
(7244 6568 7218 6542 7192 5216 4540 5866 5190 6516 4514 5892 5840 5164
7166 6490 4488 5814 7140 5138 6464 5788 3838 3864 3812 4462 3786 5112
4436 3760 )
#;> 
|#
(for-generic for7 *solutions7* *solutions8* ftest8)


(for-generic for6 *solutions6* *solutions7* ftest7)
#|
#;> (for6)
there are 30 kept / 68496 dumped / 68526 total 
#;> (show-solutions *solutions6*)
solutions => 
(172 173 174 170 171 222 223 196 197 198 199 200 252 248 249 250 251
224 225 226 276 277 278 274 275 148 144 145 146 147 )
|#


(for-generic for5 *solutions5* *solutions6* ftest6)

#|
#;> (for5)
there are 210 kept / 65400 dumped / 65610 total 
#;> (show-solutions *solutions5*)
solutions => 
(7145 7144 7143 7142 7141 7140 7139 7167 7166 5897 7165 5896 5895 5894
5893 5892 5891 5871 5870 5869 5868 5867 5866 5865 5839 5845 5844 5843
5842 5841 5840 7247 7246 7245 7244 5793 7243 5792 5819 5818 5817 5816
5815 5814 5813 7249 7248 5791 5790 5789 5788 5787 7223 7222 7221 7220
7219 7218 7217 7171 7170 7169 7168 7197 7196 7195 7194 7193 7192 7191
4545 4544 4519 4518 4517 4516 4515 4514 4513 4543 4542 4541 4540 4539
4493 4492 4491 4490 4489 4488 4487 4463 4462 4461 4467 4466 4465 4464
4441 4440 4439 4438 4437 4436 4435 6573 6572 6571 6570 6569 6568 6567
6543 6542 6541 5117 5116 5115 5114 5113 3843 5112 3842 5111 3841 3840
3869 3868 3867 3866 3865 3864 3863 6547 6546 6545 6544 3817 3816 3815
3814 3813 3812 3811 3839 3838 3837 6521 6520 6519 6518 6517 6516 6515
3791 3790 3789 3788 3787 3786 3785 6469 6468 6467 6466 6465 6464 6495
6494 6493 6492 6491 6490 5221 6489 5220 5219 5218 5217 5216 3759 5195
6463 5194 5193 5192 5191 5190 5189 3765 5215 3764 3763 3762 3761 3760
5167 5166 5165 5164 5163 5169 5168 5143 5142 5141 5140 5139 5138 5137
)

|#


(for-generic for4 *solutions4* *solutions5* ftest5)

#|
(for4)
there are 1890 kept / 57159 dumped / 59049 total 
#;> (show-solutions *solutions4*)
solutions => 
(100608 100609 100524 100525 100526 100527 100523 100528 100529 100530
100531 100504 100505 100500 100501 100502 100503 100497 100498 100499
100580 100581 100582 100583 100576 100577 100578 100579 100604 100605
100606 100607 100601 100602 100603 100556 100557 100552 100553 100554
100555 100549 100550 100551 100575 100452 100453 100448 100449 100450
100451 100476 100477 100478 100479 100472 100473 100474 100475 100471
100445 100446 100447 150573 150574 150575 150580 150581 150576 150577
150578 150579 150528 150529 150552 150553 150554 150555 150548 150549
150550 150551 150547 150632 150633 150628 150629 150630 150631 150625
150626 150627 150604 150605 150606 150607 150600 150601 150602 150603
150599 151308 151309 151304 151305 151306 151307 151301 151302 151303
151204 151205 151200 151201 151202 151203 151228 151229 151230 151231
151224 151225 151226 151227 151223 185784 185785 151176 185780 151177
185781 151178 185782 151179 185783 151172 151173 185777 151174 185778
151175 185779 151171 151197 151198 151199 185732 185733 185728 185729
185730 185731 185756 185757 185758 185759 151276 185752 151277 185753
151278 185754 151279 185755 151275 185751 151280 151281 151282 151283
151256 151257 151252 151253 151254 151255 151249 151250 151251 185647
185652 185653 185654 185655 185648 185649 185650 185651 185628 185629
151148 185624 151149 185625 151150 185626 151151 185627 151145 185621
151146 185622 151147 185623 185704 185705 185706 185707 185700 185701
185702 185703 185699 151152 151153 185725 185726 185727 185676 185677
185678 185679 185673 185674 185675 185680 185681 186300 186301 186302
186303 186297 186298 186299 186349 186350 186351 186356 186357 186352
186353 186354 186355 186304 186305 186328 186329 186330 186331 186324
186325 186326 186327 186323 150500 150501 150502 150503 150496 150497
150498 150499 150524 150525 150526 150527 150521 150522 150523 150476
150477 150472 150473 150474 150475 150469 150470 150471 150495 133028
133029 133030 133031 133024 133025 133026 133027 133052 133053 133054
133055 133049 133050 133051 133004 133005 133000 133001 133002 133003
132997 132998 132999 133023 133056 133057 132900 132901 132896 132897
132898 132899 132924 132925 132926 132927 132920 132921 132922 132923
132919 132893 132894 132895 132972 132973 132974 132975 132971 132976
132977 132978 132979 132952 132953 132948 132949 132950 132951 132945
132946 132947 187812 187813 187808 187809 187810 187811 187784 187785
187786 187787 187780 187781 187782 187783 187779 187805 187806 187807
187680 187681 187682 187683 187708 187709 187704 187705 187706 187707
187701 187702 187703 187656 187657 187652 187653 187654 187655 187649
187650 187651 187676 187677 187678 187679 187675 187756 187757 187758
187759 187753 187754 187755 116128 116129 116130 116131 116156 116157
116152 116153 116154 116155 187760 116149 187761 116150 116151 187727
116104 116105 116100 116101 116102 116103 116097 116098 116099 116124
116125 116126 116127 187732 187733 187734 116123 187735 187728 187729
187730 187731 116000 116001 116024 116025 116026 116027 116020 116021
116022 116023 116019 115996 115997 115998 115999 115993 115994 115995
116076 116077 116078 116079 116072 116073 116074 116075 116071 116045
116046 116047 116052 116053 116048 116049 116050 116051 188332 188333
188328 188329 188330 188331 188325 188326 188327 188351 188384 188385
188408 188409 188410 188411 188404 188405 188406 188407 188403 188356
188357 188358 188359 188352 188353 188354 188355 188380 188381 188382
188383 188377 188378 188379 116669 116670 116671 116728 116729 116724
116725 134408 116726 134409 116727 116721 134404 116722 134405 116723
134406 134407 134401 134402 134403 116676 116677 116672 116673 116674
116675 116700 116701 116702 116703 116696 116697 116698 116699 116695
134304 134305 134328 134329 134330 134331 134324 134325 134326 134327
134323 134276 134277 134278 134279 134272 134273 134274 134275 134300
134301 134302 134303 134297 134298 134299 134380 134381 134382 134383
134376 134377 134378 134379 134375 134349 134350 134351 134356 134357
134352 134353 134354 134355 134252 134253 134248 134249 134250 134251
134245 134246 134247 134271 135084 135085 135080 135081 135082 135083
135077 135078 135079 135052 135053 135054 135055 135051 135056 135057
135058 135059 134952 134953 134954 134955 134948 134949 134950 134951
134947 134973 134974 134975 134924 134925 134926 134927 134921 134922
134923 134928 134929 152608 152609 152632 152633 152634 152635 135032
152628 135033 152629 152630 152631 135028 135029 135030 135031 152627
135025 135026 135027 152580 152581 152582 152583 134980 152576 134981
152577 152578 152579 134976 152604 134977 152605 134978 152606 134979
152607 135004 135005 152601 135006 152602 135007 152603 135000 135001
135002 135003 134999 186408 186409 186404 186405 186406 186407 186401
186402 186403 186428 186429 186430 186431 169404 169405 186427 152653
152654 169400 152655 169401 169402 169403 169397 169398 186380 169399
186381 186382 186383 186376 186377 186378 186379 186375 152660 152661
152656 152657 152658 152659 169452 169453 169454 169455 169449 169450
169451 169456 169457 169423 186432 186433 186434 186435 186460 186461
186456 186457 186458 186459 186453 186454 186455 169428 169429 169430
169431 169424 169425 169426 169427 187136 187137 153256 153257 153258
153259 153252 153253 153254 153255 133600 153251 133601 133602 153277
133603 153278 133628 153279 133629 133624 133625 133626 133627 133621
133622 133623 153228 153229 153230 153231 153225 153226 133576 153227
133577 133572 133573 133574 133575 133569 133570 133571 133596 133597
133598 133599 133595 153232 153233 187052 187053 187054 187055 187051
153336 153337 153332 153333 153334 153335 153329 153330 153331 187056
187057 187058 153284 187059 153285 153280 153281 153282 153283 153308
153309 153310 153311 153304 153305 153306 153307 153303 187032 187033
187028 187029 187030 187031 187025 187026 187027 187108 187109 187110
187111 187104 187105 187106 187107 187132 187133 187134 187135 187129
187130 187131 187084 187085 187080 187081 187082 187083 187077 187078
187079 187103 115480 115481 153199 115476 115477 115478 115479 115473
115474 115475 153204 153205 153206 153207 153200 153201 153202 153203
153180 153181 153176 153177 153178 153179 153173 153174 153175 151980
151981 151982 151983 151977 151978 151979 115372 115373 115374 115375
186980 115369 186981 115370 115371 186976 186977 186978 186979 187004
187005 187006 151984 187007 151985 187000 187001 187002 187003 151951
186999 115376 115377 115343 151956 151957 151958 186973 151959 186974
151952 186975 151953 151954 151955 115348 115349 115350 115351 115344
115345 115346 115347 115428 115429 115424 115425 115426 115427 115452
115453 115454 115455 115448 115449 115450 115451 115447 115400 115401
115402 115403 115396 115397 115398 115399 115395 115421 115422 115423
151852 151853 151854 151855 151848 151849 151850 151851 151847 151821
151822 151823 151828 151829 151824 151825 151826 151827 151904 151905
151906 151907 151932 151933 151928 151929 151930 151931 151925 151926
151927 115324 115325 115320 115321 115322 115323 151880 151881 115317
115318 115319 151876 151877 151878 151879 151873 151874 151875 151900
151901 151902 151903 151899 118184 118185 118180 118181 118182 118183
118177 118178 118179 118156 118157 118158 118159 118152 118153 118154
118155 118151 169508 169509 169504 169505 169506 169507 169532 169533
169534 169535 169528 169529 169530 169531 169527 169480 169481 169482
169483 169476 169477 169478 169479 169475 169501 169502 169503 118052
118053 118054 118055 118048 118049 118050 118051 118076 118077 118078
118079 118073 118074 118075 118028 118029 118024 118025 118026 118027
118021 118022 118023 118047 169560 169561 169556 169557 169558 169559
169553 169554 118125 169555 118126 118127 118132 118133 118128 118129
118130 118131 118080 118081 118104 118105 118106 118107 118100 118101
118102 118103 118099 152504 152505 152500 152501 152502 152503 152497
152498 152499 152556 152557 133676 133677 152552 133678 152553 133679
152554 152555 133673 133674 152549 133675 152550 152551 152575 133680
133681 152524 152525 152526 152527 133647 152523 168204 168205 168206
168207 168201 168202 168203 133652 133653 152528 133654 152529 133655
152530 133648 152531 133649 133650 133651 133732 168208 133733 168209
133728 133729 133730 133731 133704 133705 133706 133707 133700 133701
133702 133703 133699 133725 133726 133727 168104 168105 168100 168101
168102 168103 168097 168098 168099 168124 168125 168126 168127 168123
168076 168077 168078 168079 168072 168073 168074 168075 168071 168175
168180 168181 168182 168183 168176 168177 168178 168179 168128 168129
168130 168131 168156 168157 168152 168153 168154 168155 168149 168150
168151 168045 168046 168047 168052 168053 168048 168049 168050 168051
97772 97773 97774 97775 97768 97769 97770 97771 168877 168878 168879
97767 168884 168885 97741 97742 97743 168880 168881 168882 168883
168832 168833 97748 97749 168856 97744 168857 97745 168858 97746
168859 97747 168852 168853 168854 168855 168851 168748 168749 168750
168751 168747 168752 168753 168754 168755 168728 168729 168724 168725
168726 168727 168721 168722 168723 168804 168805 168806 168807 168800
168801 168802 168803 168828 168829 168830 168831 168825 168826 168827
168780 168781 168776 168777 168778 168779 168773 168774 168775 168799
116780 116781 116776 116777 116778 116779 116773 116774 116775 116799
99772 99773 99774 99775 99769 99770 99771 116748 116749 116750 116751
116747 116752 116753 116754 116755 99821 99822 99823 116832 116833
99828 99829 99824 99825 99826 99827 116804 116805 116806 116807 116800
116801 116802 116803 116828 116829 116830 99776 116831 99777 116825
116826 116827 99800 99801 99802 99803 99796 99797 99798 99799 99795
97824 97825 97826 97827 97852 97853 97848 97849 97850 97851 97845
97846 97847 97800 97801 97796 97797 97798 97799 97793 97794 97795
97820 97821 97822 97823 97819 97900 97901 97902 97903 97897 97898
97899 97904 97905 97871 97876 97877 97878 97879 97872 97873 97874
97875 117508 117509 117504 117505 117506 117507 117423 117428 117429
117430 117431 117424 117425 117426 117427 117376 117377 117378 117379
117404 117405 117400 117401 117402 117403 117397 117398 117399 117480
117481 117482 117483 117476 117477 117478 117479 117475 117501 117502
117503 117452 117453 117454 117455 117449 117450 117451 117456 117457
117352 117353 117348 117349 117350 117351 117345 117346 117347 117372
117373 117374 117375 117371 99880 99881 99876 99877 99878 99879 99873
99874 99875 99900 99901 99902 99903 99899 99852 99853 99854 99855
99848 99849 99850 99851 99847 99904 99905 99906 99907 99932 99933
99928 99929 99930 99931 99925 99926 99927 170156 170157 170158 170159
170152 170153 170154 170155 170151 170125 170126 170127 170132 170133
170128 170129 170130 170131 170208 170209 170210 170211 170236 170237
170232 170233 170234 170235 170229 170230 170231 170184 170185 98573
98574 98575 170180 170181 170182 170183 170177 170178 170179 170204
170205 170206 170207 170203 98580 98581 98576 98577 98578 98579 98476
98477 98472 98473 98474 98475 170080 170081 98469 98470 98471 170104
170105 170106 170107 98495 170100 170101 170102 170103 170099 98444
98445 98446 98447 98443 170076 170077 170078 170079 170073 170074
170075 98448 98449 98450 98451 170912 170913 98528 98529 98552 98553
98554 98555 98548 98549 98550 98551 98547 170884 170885 170886 170887
170880 98500 170881 98501 170882 98502 170883 98503 170908 98496
170909 98497 170910 98498 170911 98499 98524 170905 98525 170906 98526
170907 98527 98521 98522 98523 170808 170809 170804 98424 170805 98425
170806 170807 98420 170801 98421 170802 98422 170803 98423 98417 98418
98419 170756 170757 170752 170753 170754 170755 170780 170781 170782
170783 170776 170777 170778 170779 170775 188460 188461 188462 170860
188463 170861 188456 188457 188458 170856 188459 170857 170858 170859
188455 170853 170854 170855 170879 99256 99257 99252 99253 99254
188429 99255 188430 170828 188431 170829 99249 170830 99250 170831
99251 170827 99204 99205 99200 99201 99202 99203 99228 99229 188436
99230 188437 99231 99224 99225 188432 99226 188433 99227 188434 170832
188435 170833 170834 170835 99223 188488 188489 188484 188485 188486
188487 188481 188482 188483 99119 170749 170750 170751 99124 99125
99126 99127 99120 99121 99122 99123 99100 99101 99096 99097 99098
99099 99093 99094 99095 99176 99177 99178 99179 99172 99173 99174
99175 99171 99197 99198 99199 99148 99149 99150 99151 99145 99146
99147 135597 135598 135599 99152 99153 135604 135605 135600 135601
135602 135603 135656 135657 135652 135653 135654 135655 135649 135650
135651 135676 135677 135678 135679 135675 135628 135629 135630 135631
135624 135625 135626 135627 135623 135727 135732 135733 135734 135735
135728 135729 135730 135731 135680 135681 135682 135683 135708 135709
135704 135705 135706 135707 135701 135702 135703 135756 135757 135758
135759 135753 135754 135755 135760 135761 )

|#

(for-generic for3 *solutions3* *solutions4* ftest4)

#|
#;> (for3)
there are 1890 kept / 4671 dumped / 6561 total 
#;> (show-solutions *solutions3*)
solutions => 
(6515 6516 6517 6518 6519 5865 5866 7248 5867 7249 5868 5869 5870 5871
3785 3786 3787 7243 3788 7244 3789 7245 3790 7246 3791 7247 5787 5788
5789 5790 5791 3837 3838 3839 3816 3817 5816 5817 5818 5819 3811 3812
6463 3813 3814 3815 7192 7193 7194 5813 7195 5814 7196 5815 7197 5792
5793 7191 7168 7169 7170 7171 3760 3761 3762 7217 3763 7218 3764 7219
3765 7220 7221 7222 7223 3759 7165 7166 7167 7144 7145 7139 7140 7141
7142 7143 4544 4545 4488 4489 4490 4491 4492 4493 4487 4539 4540 4541
4542 4543 4513 4514 4515 4516 4517 4518 4519 4440 4441 4435 4436 4437
4438 4439 4464 4465 4466 4467 5215 4461 4462 4463 5192 5193 5194 5195
5189 5190 5191 5216 5217 5218 5219 5220 5221 5137 5138 5139 5140 5141
5142 5143 5168 5169 5163 5164 5165 5166 5167 6544 6545 6546 6547 5896
5897 6541 6542 6543 5891 5892 5893 5894 5895 3864 3865 3866 3867 3868
3869 6568 6569 6570 6571 6572 6573 5112 5113 5114 3863 5115 5116 5117
6567 6489 3840 6490 3841 6491 3842 6492 3843 6493 5111 6494 6495 5840
5841 5842 5843 5844 5845 6464 5839 6465 6466 6467 6468 6469 6520 6521
)

|#

(for-generic for2 *solutions2* *solutions3* ftest3)

#|
#;> (for2)
there are 210 kept / 519 dumped / 729 total 
#;> (show-solutions *solutions2*)
solutions => 
(274 275 276 277 278 248 249 250 251 252 224 225 226 222 223 196 197 198 199 200 170 171 172 173 174 144 145 146 147 148 )
|#

(for-generic for1 *solutions1* *solutions2* ftest2)

#|
#;> (for1)
there are 30 kept / 51 dumped / 81 total 
#;> (show-solutions *solutions1*)
solutions => 
(10 9 8 7 6 5 )
|#



(format #t "loading hash table routines .....~%")
(eval '(import srfi-69))
(eval '(import (chicken format)))
(eval '(import simple-loops))
(eval '(import procedural-macros))


(format #t "starting repl ~%")
(repl)






























	   
