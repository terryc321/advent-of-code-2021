

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

;; (define (reset)
;;   (set! input (get-input "day17/input")))
;; (reset)

(format #t "trick shot loaded~%")


;; ----------- puzzle -----------------------------------

#|

integer velocity
forward + x 
upward or downward + or - y

target area
x = 20 .. 30
y = -10 .. -5

step
1 probe x position increases by probe x velocity
x = x + vx
2 probe y position increases by probe y velocity
y = y + vy
3 due drag probe x velocity decreases by 1 to zero
if vx > 0 then vx = vx - 1
4 due gravity probes y velocity decreases by 1
vy = vy - 1


|#

(define (shoot tx1 tx2 ty1 ty2 vx vy)
  (let ((x 0)
	(y 0)
	(max-y 0))
    (letrec ((step (lambda ()
		     ;; 
		     (set! x (+ x vx))
		     (set! y (+ y  vy))
		     ;; drag
		     (when (> vx 0)
		       (set! vx (- vx 1)))
		     ;; gravity
		     (set! vy (- vy 1))

		     ;; record maximum height
		     (when (> y max-y)
		       (set! max-y y))

		     (cond
		      ;; stalled 
		      ((and (< vx 1) (< x tx1))
		       #f)
		      ;; inside target region
		      ((and (>= x tx1) (<= x tx2)
			    (>= y ty1) (<= y ty2))
		       max-y )
		      ;; over-shot region
		      ((> x tx2)
		       #f)
		      ;; under-shot region
		      ((< y ty1)
		       #f)
		      ;; iterate
		      (#t (step))))))
      (step))))

#|

maximum horizontal velocity is far right hand side of target in this case 199
as vx of 200 would put probe at 200 at step 1 which is too far

minimum vy is min target area -114 , any lower and shoot below target at step 1

min horz velocity is zero

maximum y velocity ??


vx ... 0 .... 200
vy ... -114 to ???

|#

    ;;;               tx1 tx2    ty1 ty2
;; target area: x=20..30, y=-10..-5
;;   target '(20 30 -10 -5)
(define (bar target)
  (match target
    ((tx1 tx2 ty1 ty2) 
     (let ((max-y 0))
       (letrec ((foo (lambda (vx vx-max vy)
		       ;;(format #t "simulating ~a ~a ~%" vx vy)
		       (let ((val (shoot tx1 tx2 ty1 ty2 vx vy)))
			 (when
			     (and val (> val max-y))
			   (set! max-y val)
			   (format #t "max height ~a : vx ~a : vy ~a ~%" max-y vx vy))
			 (cond
			  ((> vx vx-max)
			   (foo 1 vx-max (+ vy 1)))
			  (#t (foo (+ vx 1) vx-max vy)))))))
	 (foo 0 (+ 1 tx2) (- ty1 1)))))))


(define (test-1)
  (bar '(20 30 -10 -5)))

;; problem code
;; target area: x=153..199, y=-114..-75
(define (test-2)
  (bar '(153 199 -114 -75)))
;; max height 6441 : vx 17 : vy 113 





