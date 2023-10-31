

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
(define example2 #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))


;;(set! input (get-input "day24/input"))

#|
(set! example (convert-to-2d (get-input "day24/example")))
(set! example2 (convert-to-2d (get-input "day24/example2")))

(define (reset)
   (set! input (get-input "day17/input")))
 (reset)
|#

(format #t "dice 21 loaded~%")


;; ----------- puzzle -----------------------------------

#|
problem statement
Player 1 starting position: 4
Player 2 starting position: 5

what is 1 .. 10
is that mod 11 or mod 10 ?

mod 10
0 .. 9 which is range of 10
but off by 1 ??


|#

;; deterministic dice from 1 to 100 inclusive then rolls around back to 1
(define (new-dice)
  (let ((n 1)
	(rolls 0))
    (lambda (op)
      (cond
       ((eq? op 'rolls) rolls)
       (#t 
	(let ((ans n))
	  (set! n (+ n 1))
	  (set! rolls (+ rolls 1))
	  (when (> n 100)
	    (set! n 1))
	  ans))))))

(define (slow-mod at extra)
  (letrec ((foo (lambda (n r)
		  (cond
		   ((and (> n 0) (= r 10)) (foo (- n 1) 1))
		   ((> n 0) (foo (- n 1) (+ 1 r)))
		   (#t r)))))
    (foo extra at)))


(define (make-player num start)
  (let ((pos start)
	(winner #f)
	(score 0))
    (lambda (op)
      (cond
       ((eq? op 'score) score)
       ((eq? op 'winner?) winner)       
       (#t
	(let* ((a (dice #t))
	       (b (dice #t))
	       (c (dice #t))
	       (tot (+ a b c))
	       (sum (+ pos tot))
	       (pos2 (slow-mod pos tot))) ;; SLOW MOD 
	  (set! score (+ score pos2))
	  (format #t "player ~a at [pos ~a] rolls ~a + ~a + ~a [tot ~a] and moves to space ~a for total score of ~a ~%"
		  num pos a b c tot pos2 score)
	  (set! pos pos2)
	  (cond
	   ((>= score 1000)
	    (set! winner #t)
	    (format #t "player ~a wins with score of ~a ~%" num score)
	    (format #t "dice rolls ~a ~%" (dice 'rolls)))	    
	   (#t #f))))))))



(define dice (new-dice))
(define player1 #f)
(define player2 #f)
(define (reset)
  (set! dice (new-dice)))

(define (simulate p1-start p2-start)
  (let ((player1 (make-player 1 p1-start))
	(player2 (make-player 2 p2-start)))
    (letrec ((p1-go (lambda ()
		      (player1 #t)
		      (cond
		       ((player1 'winner?)
			(format #t "player 2 loses with score of ~a ~%" (player2 'score))
			(let ((sol (* (dice 'rolls) (player2 'score))))
			  (format #t "solution = ~a ~%" sol)
			  sol))
		       (#t (p2-go)))))
	     (p2-go (lambda ()
		      (player2 #t)
		      (cond
		       ((player2 'winner?)
			(format #t "player 1 loses with score of ~a ~%" (player1 'score))
			(let ((sol (* (dice 'rolls) (player1 'score))))
			  (format #t "solution = ~a ~%" sol)
			  sol))
		       (#t (p1-go)))))
	     )
      (reset)
      (p1-go))))



(define (test-1)
  (simulate 4 8))

(define (part-1)
  (simulate 4 5))

#|
------------ results ------------

player 1 at [pos 6] rolls 25 + 26 + 27 [tot 78] and moves to space 4 for total score of 930 
player 2 at [pos 3] rolls 28 + 29 + 30 [tot 87] and moves to space 10 for total score of 1005 
player 2 wins with score of 1005 
dice rolls 930 
player 1 loses with score of 930 
solution = 864900 
$42 = 864900

|#










