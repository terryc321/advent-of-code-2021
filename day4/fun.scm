
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

(set! input (get-input "day4/input"))
(set! input2 (get-input "day4/input2"))

(define picks2 (car input2))
(define boards2 (cdr input2))

(define picks (car input))
(define boards (cdr input))

;; --------------------- macros --------------------------

#|
 5x5 boards 25 numbers 
|#

(define (rows b)
  (list (take (drop b (* 5 0)) 5)
	(take (drop b (* 5 1)) 5)
	(take (drop b (* 5 2)) 5)
	(take (drop b (* 5 3)) 5)
	(take (drop b (* 5 4)) 5)))

;;(equal? (car boards) (apply append (rows (car boards))))
#|

0  1  2  3  4
5  6  7  8  9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24

cols : reading 5x5 grid vertically now 

|#
(define (cols b)  
  (list
   (list (list-ref b 0) (list-ref b 5)(list-ref b 10)(list-ref b 15)(list-ref b 20))
   (list (list-ref b 1) (list-ref b 6)(list-ref b 11)(list-ref b 16)(list-ref b 21))
   (list (list-ref b 2) (list-ref b 7)(list-ref b 12)(list-ref b 17)(list-ref b 22))
   (list (list-ref b 3) (list-ref b 8)(list-ref b 13)(list-ref b 18)(list-ref b 23))
   (list (list-ref b 4) (list-ref b 9)(list-ref b 14)(list-ref b 19)(list-ref b 24))))

  
(define (sets b)
  (append (rows b) (cols b)))

#|

reduce a 5x5 board bingo to a set of five numbers

scheme@(guile-user) [4]> (pp (rows (car boards)))
((59 98 84 27 56)
 (17 35 18 64 34)
 (62 16 74 26 55)
 (21 99 1 19 93)
 (65 68 53 24 73))

scheme@(guile-user) [4]> (pp (cols (car boards)))
((59 17 62 21 65)
 (98 35 16 99 68)
 (84 18 74 1 53)
 (27 64 26 19 24)
 (56 34 55 93 73))

scheme@(guile-user) [4]> (pp (sets (car boards)))
((59 98 84 27 56)
 (17 35 18 64 34)
 (62 16 74 26 55)
 (21 99 1 19 93)
 (65 68 53 24 73)
 (59 17 62 21 65)
 (98 35 16 99 68)
 (84 18 74 1 53)
 (27 64 26 19 24)
 (56 34 55 93 73))

winning bingo board is the board that has fastest match on any column or row
(sets <board>) generates the list of five numbers representing each row and each column

5x5 board should have 5 rows 5 columns thats 10 entries in total 

|#


#|

given picks a sequence of numbers bingo caller calls out in sequence
how long is it until get a match given a set of 5 numbers ?

five : empty? - found all values - return score
bingo : empty? - no more values 

|#

(define (fast2 five bingo score)
  (cond
   ((null? five) score)
   ((null? bingo) 999999999999)
   ((member (car bingo) five)
    (fast2 (filter (lambda (x) (not (= x (car bingo)))) five)
	  (cdr bingo)
	  (+ score 1)))
   (#t (fast2 five (cdr bingo) (+ score 1)))))

(define (fast five bingo)
  (fast2 five bingo 0))


;; b : board
;; fives : rows columns of board 5 values
(define (fastest b bingo)
  (let ((fives (sets b)))
    (car (sort (map (lambda (five)
		      (fast five bingo))
		    fives)
	       <))))


(define (win-board boards picks)
  (let ((i 0))
    (sort
     (map (lambda (b)
	    (let ((res (list 'board i 'fastest (fastest b picks))))
	      (set! i (+ i 1))
	      res))
	  boards)
     (lambda (x y)
       (< (fourth x) (fourth y))))))


#|
scheme@(guile-user) [4]> (win-board boards picks)
$48 = ((board 63 fastest 26) (board 44 fastest 31) (board 37 fastest 38) (board 64 fastest 38) (board 39 fastest 39) (board 22 fastest 40) (board 45 fastest 40) (board 53 fastest 40) (board 94 fastest 42) (board 24 fastest 44) (board 49 fastest 45) (board 61 fastest 45) (board 57 fastest 46) (board 46 fastest 47) (board 16 fastest 48) (board 66 fastest 48) (board 33 fastest 49) (board 36 fastest 49) (board 9 fastest 50) (board 14 fastest 50) (board 20 fastest 50) (board 86 fastest 50) (board 93 fastest 50) (board 85 fastest 52) (board 87 fastest 52) (board 26 fastest 53) (board 71 fastest 53) (board 17 fastest 55) (board 21 fastest 55) (board 41 fastest 55) (board 72 fastest 55) (board 77 fastest 55) (board 92 fastest 55) (board 99 fastest 55) (board 84 fastest 56) (board 19 fastest 57) (board 51 fastest 57) (board 2 fastest 58) (board 4 fastest 58) (board 12 fastest 58) (board 62 fastest 58) (board 69 fastest 58) (board 80 fastest 59) (board 97 fastest 59) (board 5 fastest 60) (board 7 fastest 60) (board 40 fastest 60) (board 47 fastest 60) (board 70 fastest 60) (board 1 fastest 61) (board 35 fastest 61) (board 59 fastest 61) (board 79 fastest 61) (board 90 fastest 62) (board 60 fastest 63) (board 83 fastest 63) (board 3 fastest 64) (board 27 fastest 64) (board 73 fastest 64) (board 18 fastest 65) (board 78 fastest 65) (board 34 fastest 66) (board 55 fastest 66) (board 67 fastest 66) (board 10 fastest 67) (board 23 fastest 67) (board 28 fastest 67) (board 50 fastest 67) (board 95 fastest 67) (board 0 fastest 68) (board 25 fastest 68) (board 43 fastest 68) (board 58 fastest 68) (board 31 fastest 69) (board 54 fastest 69) (board 29 fastest 70) (board 52 fastest 70) (board 68 fastest 70) (board 8 fastest 71) (board 11 fastest 71) (board 38 fastest 71) (board 74 fastest 71) (board 82 fastest 71) (board 30 fastest 72) (board 42 fastest 72) (board 76 fastest 72) (board 89 fastest 72) (board 48 fastest 73) (board 75 fastest 74) (board 91 fastest 75) (board 96 fastest 75) (board 6 fastest 76) (board 15 fastest 76) (board 32 fastest 76) (board 81 fastest 77) (board 65 fastest 78) (board 98 fastest 78) (board 88 fastest 79) (board 56 fastest 85) (board 13 fastest 86))

board 63 is stated as fastest board , lets check

(board 63 fastest 26)

--check
scheme@(guile-user) [4]> (fastest (list-ref boards 63) picks)
$50 = 26

|#

(define (win) (list-ref boards 63))


#|

as sanity check we can check input2

scheme@(guile-user) [4]> (win-board boards2 picks2)
$54 = ((board 2 fastest 12) (board 0 fastest 14) (board 1 fastest 15))

|#   
(define (win2) (list-ref boards2 2))


;; almost solve problem just want to see what is last value to get the answer
;; pun  fast -> fist

(define (fist2 five bingo score)
  (cond
   ((null? five) score)
   ((null? bingo) 999999999999)
   ((member (car bingo) five)
    (fast2 (filter (lambda (x) (not (= x (car bingo)))) five)
	  (cdr bingo)
	  (+ score 1)))
   (#t (fast2 five (cdr bingo) (+ score 1)))))

(define (fist five bingo)
  (fast2 five bingo 0))
       
(define (fistest b bingo)
  (let ((fives (sets b)))
    (map (lambda (five)
	   (list five 'fist=> (fist five bingo)))
	 fives)))

#|
scheme@(guile-user) [4]> (pp (check))
(((6 26 69 27 75) fist=> 87)
 ((61 33 88 38 20) fist=> 67)
 ((9 56 70 98 82) fist=> 80)
 ((80 76 55 66 29) fist=> 75)
 ((97 84 42 77 73) fist=> 26) .........
 ((6 61 9 80 97) fist=> 53)
 ((26 33 56 76 84) fist=> 80)
 ((69 88 70 55 42) fist=> 33)
 ((27 38 98 66 77) fist=> 59)
 ((75 20 82 29 73) fist=> 87))


scheme@(guile-user) [5]> (pp (rows (win)))
((6 26 69 27 75)
 (61 33 88 38 20)
 (9 56 70 98 82)
 (80 76 55 66 29)
 (97 84 42 *77* 73)) ......... last row of (win) 

scheme@(guile-user) [5]> (take picks 26)
$67 = (17 58 52 49 72 33 55 73 27 69 88 80 9 7 59 98 63 42 84 37 87 28 97 66 79 77)
                                                                                ^^
believe *77* was the last number to be selected in the bingo

|#

(define (check)
  (fistest (win) picks))


#|
all numbers in (win) that are no in (take picks 26)
|#

(define (bar2 vals bingo acc)
  (cond
   ((null? vals) acc)
   ((member (car vals) bingo)
    (bar2 (cdr vals) bingo acc))
   (#t (bar2 (cdr vals) bingo (cons (car vals) acc)))))

(define (bar)
  (let ((vals (win))
	(bingo (take picks 26)))
    (bar2 vals bingo '())))

(define (sv)
  (apply + (bar)))


(define (part-1)
  (* (sv) (last (take picks 26))))

#|

scheme@(guile-user) [5]> (part-1)
$77 = 41503

|#



#|

piece of mind - peace - work the example code also

3rd board - (2nd if index from 0 1 *2* list-ref boards2 2)
winning value is 24 

scheme@(guile-user) [6]> (pp (rows (win2)))
((14 21 17 24 4)
 (10 16 15 9 19)
 (18 8 23 26 20)
 (22 11 13 6 5)
 (2 0 12 3 7))
scheme@(guile-user) [6]> (take picks2 12)
$81 = (7 4 9 5 11 17 23 2 0 14 21 24)
                      winning val ^^ 

(last (take picks2 12)) = 24

(apply + (bar2 (win2)
		    (take picks2 12)
'())) = 188

(* 188 24) = 4512

|#
(define (example-1)
  (* (apply + (bar2 (win2)
		    (take picks2 12)
		    '()))
     (last (take picks2 12))))


  







