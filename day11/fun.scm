

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
(define example #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))

(set! input (get-input "day11/input"))
(set! example (get-input "day11/example"))

;; -------------------- another 2 d puzzle ---------------------------
#|
sure written this before somewhere 
increase energy of all octopuses by 1 one
flash increase energy of all octopuses surrounding it by 1 including diagonals
any that flash have energy reset to 0 zero
|#

;; not a proper deep vector copy ....
;; (define v2 (vector-copy input))
;; (vector-set! (vector-ref v2 0) 0 123)

(define flashes 0)

(define (reset)
  (set! flashes 0))


(define (deep-copy vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec)))
    (let ((vert (make-vector height #f)))
      (dolist (y (iota height))
	      (let ((horz (make-vector width 0)))
		(dolist (x (iota width))
			(vector-set! horz x (vector-ref (vector-ref vec y) x)))
		(vector-set! vert y horz)))
      vert)))

(define (increment vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec)))
    (dolist (y (iota height))
	    (dolist (x (iota width))
		    (set-xy x y (+ 1 (get-xy x y vec)) vec)))
    vec))


;; 2d get + set using vector in 2d
(define (get-xy x y vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec)))
    (assert (>= x 0))
    (assert (< x width))
    (assert (>= y 0))
    (assert (< y height))
    (vector-ref (vector-ref vec y) x)))

(define (set-xy x y z vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec)))
    (assert (>= x 0))
    (assert (< x width))
    (assert (>= y 0))
    (assert (< y height))
    (vector-set! (vector-ref vec y) x z)))

(define (onboard x y vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec)))
    (and (>= x 0) (< x width) (>= y 0) (< y height))))

#|
if any octopus reaches a 9 then we will flash that one and set its value to false
so we know it has flashed
when all octopus have finished flashing then we will scrape all above 9 values and reset them
to zero 
ready for the next step in puzzle
|#

(define-macro (inc-neighbour dx dy vec)
  `(when (onboard ,dx ,dy ,vec)
     (let ((value (get-xy ,dx ,dy ,vec)))
       (cond
	((integer? value) (set-xy ,dx ,dy (+ 1 value) ,vec))
	(#t #f)))))

  
#|
(let ((dx (- x 1))
	(dy y))
    (when (onboard dx dy) (set-xy dx dy (+ 1 (get-xy dx dy vec)) vec)))

but this is repeated for 9 cases 
(- x 1) (+ y 1)
x (+ y 1)
(+ x 1) (+ y 1)
(- x 1) y
x y
(+ x 1) y
(- x 1) (- y 1)
x (- y 1)
(- x 1) (- y 1)

|#

(define (increment-neighbours x y vec)
  (inc-neighbour (- x 1) (+ y 1) vec)
  (inc-neighbour (+ x 1) (+ y 1) vec)
  (inc-neighbour x (+ y 1) vec)
  ;;
  (inc-neighbour (- x 1) y vec)
  (inc-neighbour (+ x 1) y vec)
  (inc-neighbour x y vec)
  ;;
  (inc-neighbour (- x 1) (- y 1) vec)
  (inc-neighbour (+ x 1) (- y 1) vec)
  (inc-neighbour x (- y 1) vec)
  )

;; remove #f false values from vec

(define (scan vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec))
	(change #f))
    (dolist (y (iota height))
	    (dolist (x (iota width))
		    (cond
		     ((eq? (get-xy x y vec) #f) #f)
		     ((> (get-xy x y vec) 9)
		      (set-xy x y #f vec)
		      (set! change #t)
		      (increment-neighbours x y vec))
		     (#t #f))))
    (cond
     (change (scan vec))
     (#t vec))))


(define (reset-flashed vec)
  (let ((width (vector-length (vector-ref vec 0)))
	(height (vector-length vec)))
    (dolist (y (iota height))
	    (dolist (x (iota width))
		    (cond
		     ((eq? (get-xy x y vec) #f)
		      (set! flashes (+ 1 flashes))
		      (set-xy x y 0 vec))
		     (#t #f))))
    vec))

  
(define (step vec)
  (let ((vec2 (deep-copy vec)))
    (increment vec2)
    (scan vec2)
    (reset-flashed vec2)
    vec2))





;; ---------------- test cases -----------------------
(define (test-1)
  (define v2 (deep-copy input))
  (vector-set! (vector-ref v2 0) 0 123)
  (pp v2)
  (pp input))


(define (test-2)
  (reset)
  (define (foo n vec)
    (cond
     ((= n 0) (pp vec) flashes)
     (#t (foo (- n 1) (step vec)))))
  (foo 100 example))

(define (pass-1)
  (reset)
  (define (foo n vec)
    (cond
     ((= n 0) (pp vec) flashes)
     (#t (foo (- n 1) (step vec)))))
  (foo 100 input))


#|
scheme@(guile-user) [3]> (pass-1)
#(#(1 2 0 0 2 1 1 1 1 6)
  #(1 2 3 9 8 1 1 1 1 1)
  #(1 1 1 6 5 8 1 1 1 1)
  #(1 7 7 5 5 8 1 1 1 1)
  #(1 7 5 4 6 5 1 1 1 1)
  #(5 5 4 5 8 6 1 1 1 1)
  #(8 4 5 7 0 8 2 1 1 1)
  #(6 7 9 0 0 0 3 1 2 2)
  #(5 5 6 9 0 0 3 2 4 0)
  #(5 5 5 7 3 3 2 2 0 0))

$27 = 1673

|#
  




;; ---------------- results ---------------------------


















