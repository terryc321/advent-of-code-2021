
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

(set! input (get-input "day5/input"))
(set! input2 (get-input "day5/input2"))

#|
file format of input
(x1 y1 x2 y1)
include end points 

this will generate a set of points

anywhere there is more than one point HIT we count as ONE
what is the total count ?

assume only vertical lines /\  \/  x1 = x2
assume only horizontal lines <  ... >  y1 = y2

|#

;; 
(define points #f)
(define counts 0)

(define (reset)
  (set! points (make-hash-table))
  (set! counts 0))


#|
enter point (x y) into hash with value of 1 if (x y) not in hash
if in hash - increment point
|#
(define (hit-point x y)
  (let* ((key (list x y))
	 (val (hash-ref points key)))
    (format #t "hit-point.val = ~a ~%" val)
    (cond
     (val (hash-set! points key (+ val 1)))	  
     (#t (hash-set! points key 1)))))


;; y1 <= y2
(define (vertical x y1 y2)
  (format #t "vertical point x:~a y:~a ~%" x y1)
  (cond
   ((= y1 y2) (hit-point x y1))
   (#t (hit-point x y1)
       (vertical x (+ y1 1) y2))))

;; x1 <= x2
(define (horizontal x1 x2 y)  
  (format #t "horizontal x:~a y:~a ~%" x1 y)
  (cond
   ((= x1 x2) (hit-point x1 y))
   (#t (hit-point x1 y)
       (horizontal (+ x1 1) x2 y))))


(define (foo2 x1 y1 x2 y2)
  (cond
   ((= x1 x2) (vertical x1 (min y1 y2) (max y1 y2)))
   ((= y1 y2) (horizontal (min x1 x2) (max x1 x2) y1))
   (#t
    ;;(error "foo2.neither horz nor vert" (list x1 y1 x2 y2))
    #f
    )))

(define (foo xs)
  (match xs
    ((x1 y1 x2 y2) (foo2 x1 y1 x2 y2))
    ( _ (error "foo.match fail" (list xs)))))


(define (example-1)
  (reset)
  (map foo input2)
  (count-em))


(define (count-em)
  (let ((count 0))
    (hash-for-each (lambda (k v)
		     (cond
		      ((> v 1)
		       (format #t "~a ~%" v)
		       (set! count (+ count 1)))
		      (#t #f)))
		   points)
    count))
			  
		   
(define (part-1)
  (reset)
  (map foo input)
  (count-em))


#|

2 
2 
$1 = 6267
scheme@(guile-user)> 


|#


	

