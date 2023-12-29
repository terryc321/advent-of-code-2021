
;;(import (lsp-server))

(import scheme)
(import (chicken process-context))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken random))
;;(define pp pretty-print)

;;(import (chicken doc))
;; documentation

;; debugging macro expander
;; debugger

(import procedural-macros)
(import regex)


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


#|
find ourselves writing the same routines over and over again

1) read file
2) parse file - or rather not parse - then using reg expressions to make up for fact
cannot parse file
3) try grep and fail miserably

|#

(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((lines '()))
	(letrec ((loop (lambda ()
			 (let ((in (read-line port)))
			   (cond
			    ((eof-object? in) (reverse lines))
			    (#t (set! lines (cons in lines))
				(loop)))))))
	  (loop))))))



(define input (read-lines "input"))

#|
regexs
vriables w x y z 
-----------------------
inp a - Read an input value and write it to variable a.
inp var     --------   m10
where var is only w in input code
------------------------
mul a b - Multiply the value of a by the value of b, then store the result in variable a.
mul var var    ------ m20
mul var int    ------ m21
--------------
add a b - Add the value of a to the value of b, then store the result in variable a.

add var var     ---- m40
add var int     ---- m41
-----------------------

div a b -
Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)

div var int -------- m50

------------------

mod a b -
Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)

mod var int  --------- m60

----------------------

eql a b -
If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.

eql
add var var     ---- m70
add var int     ---- m71

eql x 0
eql x w

--------------------

|#


(define (regs)
  (let ((n 0)
	(res '())
	(unmatched '()))

    (define (sym str)
      (let ((s (string->symbol str)))
	(assert (member s '(w x y z) eq?))
	s))
    (define (int str)
      (let ((s (string->number str)))
	(assert (integer? s))
	s))
    
    (define (sym2 x)
      (sym (second x)))
    (define (sym3 x)
      (sym (third x)))
    (define (int2 x)
      (int (second x)))
    (define (int3 x)
      (int (third x)))
    
    
    (define (inp-1 line)
      (let ((m (string-match "inp ([wxyz])" line)))
	(cond
	 (m
	  (set! res (cons `(inp ,(sym2 m)) res))
	  #t)
	 (#t #f))))
    
    (define (mul-1 line) 
      (let ((m (string-match "mul ([wxyz]) ([wxyz])" line)))
	(cond
	 (m
	  (set! res (cons `(mul ,(sym2 m) ,(sym3 m)) res))
	  #t)
	 (#t #f))))
	
    (define (mul-2 line)
      (let ((m (string-match "mul ([wxyz]) ([+\\-]?[0-9]+)" line)))
	(cond
	 (m
	     (set! res (cons `(mul ,(sym2 m) ,(int3 m)) res))
	  #t)
	 (#t #f))))

    (define (add-1 line) 
      (let ((m (string-match "add ([wxyz]) ([wxyz])" line)))
	(cond
	 (m
	  (set! res (cons `(add ,(sym2 m) ,(sym3 m))  res))
	  #t)
	 (#t #f))))
	
    (define (add-2 line)
      (let ((m (string-match "add ([wxyz]) ([+\\-]?[0-9]+)" line)))
	(cond
	 (m
	  (set! res (cons `(add ,(sym2 m)  ,(int3 m)) res))
	  #t)
	 (#t #f))))

    (define (div-1 line) 
      (let ((m (string-match "div ([wxyz]) ([+\\-]?[0-9]+)" line)))
	(cond
	 (m
	  (set! res (cons `(div ,(sym2 m) ,(int3 m)) res))
	  #t)
	 (#t #f))))

    (define (mod-1 line) 
      (let ((m (string-match "mod ([wxyz]) ([+\\-]?[0-9]+)" line)))
	(cond
	 (m
	  (set! res (cons `(mod ,(sym2 m) ,(int3 m)) res))
	  #t)
	 (#t #f))))
    
    (define (eql-1 line) 
      (let ((m (string-match "eql ([wxyz]) ([wxyz])" line)))
	(cond
	 (m
	  (set! res (cons `(eql ,(sym2 m) ,(sym3 m)) res))
	  #t)
	 (#t #f))))
	
    (define (eql-2 line)
      (let ((m (string-match "eql ([wxyz]) ([+\\-]?[0-9]+)" line)))
	(cond
	 (m
	  (set! res (cons `(eql ,(sym2 m) ,(int3 m)) res))
	  #t)
	 (#t #f))))

    (define (catch-all line)
      (set! unmatched (cons line unmatched))
      (error "catch-all" (list line))
      #t)
    
    
    (do-list (line input)
	     (or (inp-1 line)
		 (mul-1 line)
		 (mul-2 line)
		 (add-1 line)
		 (add-2 line)
		 (div-1 line)
		 (mod-1 line)
		 (eql-1 line)
		 (eql-2 line)
		 (catch-all line)
		 ))
		 
    (format #t "~%~%------------~%~%")
    (pp unmatched)
    (format #t "number of unmatched lines = ~a ~%" (length unmatched))
    (reverse res)))


(define input (regs))

;; take a list and split it on (inp _)
;; assume list starts wit h(inp _) keep eating till find another (inp _) or end of list
(define (grepper prog)  
  (define (keat2 xs)
    (cond
     ((null? xs) '())
     ((eq? (car (car xs)) 'inp) '())
     (#t (cons (car xs) (keat2 (cdr xs))))))
  (define (keat xs)
    (cond
     ((null? xs) '())
     ((eq? (car (car xs)) 'inp) (cons (car xs) (keat2 (cdr xs))))
     (#t (error "keat" (list xs)))))
  (define res '())
  (define (loopy ys)
    (cond
     ((null? ys) res)
     (#t (let* ((a (keat ys))
		(len-a (length a)))
	   (set! res (cons a res))
	   (loopy (drop ys len-a))))))
  (loopy prog)
  (reverse res))

(define input-parts (grepper input))

(define (get-input-part n)
  (list-ref input-parts (- n 1)))


(define p1 (equal? '(			    (inp w)     ;; w = n1    ............ line 1
			    (mul x 0)   ;; ............ x = 0
			    (add x z)   ;; x = 0
			    (mod x 26)  ;; x = 0  : mod 0 26 = 0
			    (div z 1)   ;; z = 0  : div 0 1 is 0 
			    (add x 10)  ;; x = 10 : (+ 10 (mod z 26))    ...... here z = { 1 ..9 inclusive }
			    (eql x w)   ;; x = 0  : always FALSE x = 0 x = w ?? x = 1 or x = 0     [ (+ 10 (mod z 26)) = 1 .. 9 ?? ]
			    (eql x 0)   ;; x = 1  : 0  =  0
			    (mul y 0)   ;; y = 0
			    (add y 25)  ;; y = 25
			    (mul y x)   ;; y = 25 : x = 1 * x  : x = 1 : so y = 25
			    (add y 1)   ;; y = 26 : (+ 1 (* 25 x))   so y = 26
			    (mul z y)   ;; z = 0  : (* z (+ 1 (* 25 x))
			    (mul y 0)   ;; y = 0
			    (add y w)   ;; y = n1 :  
			    (add y 1)   ;; y = n1 + 1 
			    (mul y x)   ;; y = n1 + 1
			    (add z y)   ;; z = n1 + 1 : from y = 0 .......   line 18
			    )
		   (get-input-part 1)))




#|



#;2868> (num->ilist 1232.0)
(1 2 3 2 -2 0)
#;2874> (= 1232 1232.0)
#t
#;2886> (num->ilist 1232.0)
(1 2 3 2 -2 0)
#;2890> 
|#

(define (num->ilist x)
  (cond
   ((string? x)
    (map (lambda (x) (- x (char->integer #\0))) (map char->integer (string->list x))))
   ((integer? x)
    (cond
     ((< x 0) (error "num->ilist" (list x 'negative 'integer)))
     (#t 
      (num->ilist (format #f "~a" x)))))
   (#t
    (error "num->ilist" (list x 'unrecognised 'type 'expected 'number 'or 'list)))))




(define (exec vars data prog)
  (let ((w (second (assq 'w vars )))
	(x (second (assq 'x vars )))
	(y (second (assq 'y vars )))
	(z (second (assq 'z vars)))
	)
    (define debug #f)
    (define (run xs)
      (cond
       ((null? xs) `((w ,w)(x ,x)(y ,y)(z ,z)))
       (#t (run-command (car xs))
	   (run (cdr xs)))))

    (define (run-command asm)
      (when debug
	(format #t "~a : ~a " `((w ,w)(x ,x)(y ,y)(z ,z)) asm)
	(cond
	 ((eq? (car asm) 'inp) (format #t " : inp-data ~a : " data))))
      
      (cond
       ((eq? (car asm) 'inp) (run-inp asm))
       ((eq? (car asm) 'add) (run-add asm))
       ((eq? (car asm) 'mul) (run-mul asm))
       ((eq? (car asm) 'div) (run-div asm))
       ((eq? (car asm) 'mod) (run-mod asm))
       ((eq? (car asm) 'eql) (run-eql asm))
       (#t (error "run-command" (list asm))))

      (when debug
	(format #t " : ~a ~%" `((w ,w)(x ,x)(y ,y)(z ,z)))
	)
      );; run-command
    
    (define (run-inp asm)
      (let ((var (second asm))
	    (val (car data)))
	(set! data (cdr data))
	(cond
	 ((eq? var 'w) (set! w val))
	 ((eq? var 'x) (set! x val))
	 ((eq? var 'y) (set! y val))
	 ((eq? var 'z) (set! z val))
	 (#t (error "run-inp" var val)))))

    (define (run-add asm)
      (let ((var (second asm))
	    (val (third asm))
	    (var2 #f))
	(cond
	 ((symbol? val) (set! var2 val)
	  (cond
	   ((and (eq? var 'w) (eq? var2 'w)) (set! w (+ w w )))
	   ((and (eq? var 'x) (eq? var2 'w)) (set! x (+ x w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'y) (eq? var2 'w)) (set! y (+ y w))) ;; BUG HERE set x instead of y !!!
	   ((and (eq? var 'z) (eq? var2 'w)) (set! z (+ z w)))

	   ((and (eq? var 'w) (eq? var2 'x)) (set! w (+ w x)))
	   ((and (eq? var 'x) (eq? var2 'x)) (set! x (+ x x)))
	   ((and (eq? var 'y) (eq? var2 'x)) (set! y (+ y x)))
	   ((and (eq? var 'z) (eq? var2 'x)) (set! z (+ z x)))

	   ((and (eq? var 'w) (eq? var2 'y)) (set! w (+ w y)))
	   ((and (eq? var 'x) (eq? var2 'y)) (set! x (+ x y)))
	   ((and (eq? var 'y) (eq? var2 'y)) (set! y (+ y y)))
	   ((and (eq? var 'z) (eq? var2 'y)) (set! z (+ z y)))

	   ((and (eq? var 'w) (eq? var2 'z)) (set! w (+ w z)))
	   ((and (eq? var 'x) (eq? var2 'z)) (set! x (+ x z)))
	   ((and (eq? var 'y) (eq? var2 'z)) (set! y (+ y z)))
	   ((and (eq? var 'z) (eq? var2 'z)) (set! z (+ z z)))

	   (#t (error "run-add alpha" var val))))
	 ((integer? val)
	  (cond
	   ((eq? var 'w)  (set! w (+ w val)))
	   ((eq? var 'x)  (set! x (+ x val)))
	   ((eq? var 'y)  (set! y (+ y val)))
	   ((eq? var 'z)  (set! z (+ z val)))
	   (#t (error "run-add beta" var val))))
	 (#t (error "run-add charlie" var val)))))

    
    (define (run-mul asm)
      (let ((var (second asm))
	    (val (third asm))
	    (var2 #f))
	(cond
	 ((symbol? val) (set! var2 val)
	  (cond
	   ((and (eq? var 'w) (eq? var2 'w)) (set! w (* w w )))
	   ((and (eq? var 'x) (eq? var2 'w)) (set! x (* x w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'y) (eq? var2 'w)) (set! y (* y w))) ;; BUG HERE set x instead of y !!!
	   ((and (eq? var 'z) (eq? var2 'w)) (set! z (* z w)))

	   ((and (eq? var 'w) (eq? var2 'x)) (set! w (* w x)))
	   ((and (eq? var 'x) (eq? var2 'x)) (set! x (* x x)))
	   ((and (eq? var 'y) (eq? var2 'x)) (set! y (* y x)))
	   ((and (eq? var 'z) (eq? var2 'x)) (set! z (* z x)))

	   ((and (eq? var 'w) (eq? var2 'y)) (set! w (* w y)))
	   ((and (eq? var 'x) (eq? var2 'y)) (set! x (* x y)))
	   ((and (eq? var 'y) (eq? var2 'y)) (set! y (* y y)))
	   ((and (eq? var 'z) (eq? var2 'y)) (set! z (* z y)))

	   ((and (eq? var 'w) (eq? var2 'z)) (set! w (* w z)))
	   ((and (eq? var 'x) (eq? var2 'z)) (set! x (* x z)))
	   ((and (eq? var 'y) (eq? var2 'z)) (set! y (* y z)))
	   ((and (eq? var 'z) (eq? var2 'z)) (set! z (* z z)))

	   (#t (error "run-mul alpha" var val))))
	 ((integer? val)
	  (cond
	   ((eq? var 'w)  (set! w (* w val)))
	   ((eq? var 'x)  (set! x (* x val)))
	   ((eq? var 'y)  (set! y (* y val)))
	   ((eq? var 'z)  (set! z (* z val)))
	   (#t (error "run-mul beta" var val))))
	 (#t (error "run-mul charlie" var val)))))


    (define (run-div asm)
      (define (f/ x y) (floor (/ x y)))
      (let ((var (second asm))
	    (val (third asm))
	    (var2 #f))
	(cond
	 ((symbol? val) (set! var2 val)
	  (cond
	   ((and (eq? var 'w) (eq? var2 'w)) (set! w (f/ w w )))
	   ((and (eq? var 'x) (eq? var2 'w)) (set! x (f/ x w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'y) (eq? var2 'w)) (set! y (f/ y w))) ;; BUG HERE set x instead of y !!!
	   ((and (eq? var 'z) (eq? var2 'w)) (set! z (f/ z w)))

	   ((and (eq? var 'w) (eq? var2 'x)) (set! w (f/ w x)))
	   ((and (eq? var 'x) (eq? var2 'x)) (set! x (f/ x x)))
	   ((and (eq? var 'y) (eq? var2 'x)) (set! y (f/ y x)))
	   ((and (eq? var 'z) (eq? var2 'x)) (set! z (f/ z x)))

	   ((and (eq? var 'w) (eq? var2 'y)) (set! w (f/ w y)))
	   ((and (eq? var 'x) (eq? var2 'y)) (set! x (f/ x y)))
	   ((and (eq? var 'y) (eq? var2 'y)) (set! y (f/ y y)))
	   ((and (eq? var 'z) (eq? var2 'y)) (set! z (f/ z y)))

	   ((and (eq? var 'w) (eq? var2 'z)) (set! w (f/ w z)))
	   ((and (eq? var 'x) (eq? var2 'z)) (set! x (f/ x z)))
	   ((and (eq? var 'y) (eq? var2 'z)) (set! y (f/ y z)))
	   ((and (eq? var 'z) (eq? var2 'z)) (set! z (f/ z z)))

	   (#t (error "run-div alpha" var val))))
	 ((integer? val)
	  (cond
	   ((eq? var 'w)  (set! w (f/ w val)))
	   ((eq? var 'x)  (set! x (f/ x val)))
	   ((eq? var 'y)  (set! y (f/ y val)))
	   ((eq? var 'z)  (set! z (f/ z val)))
	   (#t (error "run-div beta" var val))))
	 (#t (error "run-div charlie" var val)))))


    (define (run-mod asm)
      (define (fmod x y) (modulo x y))
      (let ((var (second asm))
	    (val (third asm))
	    (var2 #f))
	(cond
	 ((symbol? val) (set! var2 val)
	  (cond
	   ((and (eq? var 'w) (eq? var2 'w)) (set! w (fmod w w )))
	   ((and (eq? var 'x) (eq? var2 'w)) (set! x (fmod x w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'y) (eq? var2 'w)) (set! y (fmod y w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'z) (eq? var2 'w)) (set! z (fmod z w)))

	   ((and (eq? var 'w) (eq? var2 'x)) (set! w (fmod w x)))
	   ((and (eq? var 'x) (eq? var2 'x)) (set! x (fmod x x)))
	   ((and (eq? var 'y) (eq? var2 'x)) (set! y (fmod y x)))
	   ((and (eq? var 'z) (eq? var2 'x)) (set! z (fmod z x)))

	   ((and (eq? var 'w) (eq? var2 'y)) (set! w (fmod w y)))
	   ((and (eq? var 'x) (eq? var2 'y)) (set! x (fmod x y)))
	   ((and (eq? var 'y) (eq? var2 'y)) (set! y (fmod y y)))
	   ((and (eq? var 'z) (eq? var2 'y)) (set! z (fmod z y)))

	   ((and (eq? var 'w) (eq? var2 'z)) (set! w (fmod w z)))
	   ((and (eq? var 'x) (eq? var2 'z)) (set! x (fmod x z)))
	   ((and (eq? var 'y) (eq? var2 'z)) (set! y (fmod y z)))
	   ((and (eq? var 'z) (eq? var2 'z)) (set! z (fmod z z)))

	   (#t (error "run-mod alpha" var val))))
	 ((integer? val)
	  (cond
	   ((eq? var 'w)  (set! w (fmod w val)))
	   ((eq? var 'x)  (set! x (fmod x val)))
	   ((eq? var 'y)  (set! y (fmod y val)))
	   ((eq? var 'z)  (set! z (fmod z val)))
	   (#t (error "run-mod beta" var val))))
	 (#t (error "run-mod charlie" var val)))))

    
    (define (run-eql asm)
      (define (feql x y) (if (= x y) 1 0))
      (let ((var (second asm))
	    (val (third asm))
	    (var2 #f))
	(cond
	 ((symbol? val) (set! var2 val)
	  (cond
	   ((and (eq? var 'w) (eq? var2 'w)) (set! w (feql w w )))
	   ((and (eq? var 'x) (eq? var2 'w)) (set! x (feql x w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'y) (eq? var2 'w)) (set! y (feql y w))) ;; BUG HERE set y instead of x !!!
	   ((and (eq? var 'z) (eq? var2 'w)) (set! z (feql z w)))

	   ((and (eq? var 'w) (eq? var2 'x)) (set! w (feql w x)))
	   ((and (eq? var 'x) (eq? var2 'x)) (set! x (feql x x)))
	   ((and (eq? var 'y) (eq? var2 'x)) (set! y (feql y x)))
	   ((and (eq? var 'z) (eq? var2 'x)) (set! z (feql z x)))

	   ((and (eq? var 'w) (eq? var2 'y)) (set! w (feql w y)))
	   ((and (eq? var 'x) (eq? var2 'y)) (set! x (feql x y)))
	   ((and (eq? var 'y) (eq? var2 'y)) (set! y (feql y y)))
	   ((and (eq? var 'z) (eq? var2 'y)) (set! z (feql z y)))

	   ((and (eq? var 'w) (eq? var2 'z)) (set! w (feql w z)))
	   ((and (eq? var 'x) (eq? var2 'z)) (set! x (feql x z)))
	   ((and (eq? var 'y) (eq? var2 'z)) (set! y (feql y z)))
	   ((and (eq? var 'z) (eq? var2 'z)) (set! z (feql z z)))

	   (#t (error "run-eql alpha" var val))))
	 ((integer? val)
	  (cond
	   ((eq? var 'w)  (set! w (feql w val)))
	   ((eq? var 'x)  (set! x (feql x val)))
	   ((eq? var 'y)  (set! y (feql y val)))
	   ((eq? var 'z)  (set! z (feql z val)))
	   (#t (error "run-eql beta" var val))))
	 (#t (error "run-eql charlie" var val)))))
    
    (run prog)))       



#|

task is to find fourteen digit number that outputs a 1 on z variable

(string-length "01234567890123")

only allowed digits 1 through 9

(length '(1 2 3 4 5 6 7 8 9))
(pow 9 14)

(expt 9 14)

22876792454961


|#

#|
n1 = 1 : z = (w 1 x 1 y 1 z 1) 
n1 = 2 : z = (w 2 x 2 y 2 z 2) 
n1 = 3 : z = (w 3 x 3 y 3 z 3) 
n1 = 4 : z = (w 4 x 4 y 4 z 4) 
n1 = 5 : z = (w 5 x 5 y 5 z 5) 
n1 = 6 : z = (w 6 x 6 y 6 z 6) 
n1 = 7 : z = (w 7 x 7 y 7 z 7) 
n1 = 8 : z = (w 8 x 8 y 8 z 8) 
n1 = 9 : z = (w 9 x 9 y 9 z 9) 

fourthteen numbers unknown
n1 .............. n14


starts
x = 0
w = 0
y = 0
z = 0

only inputs control are n1 ... n14 for code

|#
(define (test-1)
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(vars '((w 0) (x 0) (y 0) (z 0))))
    (do-list (n1 vals)
	     (let ((out
		    (exec vars
			  `(,n1)
			  '(
			    (inp w)     ;; w = n1    ............ line 1
			    (mul x 0)   ;; ............ x = 0
			    (add x z)   ;; x = 0
			    (mod x 26)  ;; x = 0  : mod 0 26 = 0
			    (div z 1)   ;; z = 0  : div 0 1 is 0 
			    (add x 10)  ;; x = 10 : (+ 10 (mod z 26))    ...... here z = { 1 ..9 inclusive }
			    (eql x w)   ;; x = 0  : always FALSE x = 0 x = w ?? x = 1 or x = 0     [ (+ 10 (mod z 26)) = 1 .. 9 ?? ]
			    (eql x 0)   ;; x = 1  : 0  =  0
			    (mul y 0)   ;; y = 0
			    (add y 25)  ;; y = 25
			    (mul y x)   ;; y = 25 : x = 1 * x  : x = 1 : so y = 25
			    (add y 1)   ;; y = 26 : (+ 1 (* 25 x))   so y = 26
			    (mul z y)   ;; z = 0  : (* z (+ 1 (* 25 x))
			    (mul y 0)   ;; y = 0
			    (add y w)   ;; y = n1 :  
			    (add y 1)   ;; y = n1 + 1 
			    (mul y x)   ;; y = n1 + 1
			    (add z y)   ;; z = n1 + 1 : from y = 0 .......   line 18
			    )
			  )))
	       (let ((wout (second (assq 'w out)))
		     (xout (second (assq 'x out)))
		     (yout (second (assq 'y out)))
		     (zout (second (assq 'z out))))
		 ;;(format #t "n1 = ~a : out = ~a ~%" n1 out)
		 (assert (= wout n1))
		 (assert (= xout 1))
		 (assert (= yout (+ n1 1)))
		 (assert (= zout (+ n1 1)))
		 )))))



;; follows from test-1 results ...... provided evaluator is correct .....
;; w = n1
;; x = 1
;; y = n1 + 1
;; z = n1 + 1
(define (test-2)  
  (let ((vals '(1 2 3 4 5 6 7 8 9)))
    (do-list (n1 vals)
	     (let ((vars `((w ,n1)(x 1)(y ,(+ 1 n1))(z ,(+ n1 1)))))
	       (do-list (n2 vals)
			(let ((out
			       (exec
				vars
				`(,n2)
				'(
				  (inp w)     ;; w = n2
				  (mul x 0)   ;; x = 0
				  (add x z)   ;; x = z  : n1 + 1
				  (mod x 26)  ;; x = mod (n1 + 1) 26
				  (div z 1)   ;; z
				  (add x 11)  ;; x = n1 + 12 : x = n1 + 1 + 11 : x = x + 11 
				  (eql x w)   ;; x = 0 : FALSE : n2 limited to 1 .. 9 : x = w ? n1 + 12 = n2 ? 
				  (eql x 0)   ;; x = 1
				  (mul y 0)   ;; y = 0
				  (add y 25)  ;; y = 25
				  (mul y x)   ;; y = 25 : y = 25 * 1
				  (add y 1)   ;; y = 26 
				  (mul z y)   ;; z = (n1 + 1) * 26 : z = n1 + 1
				  (mul y 0)   ;; y = 0
				  (add y w)   ;; y = w = n2
				  (add y 9)   ;; y = n2 + 9
				  (mul y x)   ;; y = n2 + 9
				  (add z y)   ;; z = ((n1 + 1)*26) + n2 + 9
				  )   
				)))
			  (let ((wout (second (assq 'w out)))
				(xout (second (assq 'x out)))
				(yout (second (assq 'y out)))
				(zout (second (assq 'z out))))
			    ;;(format #t "n1 = ~a : out = ~a ~%" n1 out)
			    (assert (= zout (+ n2 (* 26 n1) 35)));; (+ 9 n2 (* 26 (+ n1 1)))))
			    (assert (= xout 1))
			    (assert (= yout (+ n2 9)))
			    (assert (= wout n2))
			    )))))))


;; from test-2
;; w = n2
;; x = 1
;; y = n2 + 9
;; z = (+ n2 (* 26 n1) 35)

(define (test-3)  
  (let ((vals '(1 2 3 4 5 6 7 8 9)))
    (do-list (n1 vals)
	     (do-list (n2 vals)
		      (do-list (n3 vals)
			       (let ((vars `((w ,n2)
					     (x 1)
					     (y ,(* 9 n2))
					     (z ,(+ n2 (* 26 n1) 35)))))
				 (let ((out
					(exec
					 vars
					 `(,n3)
					 '(
					   (inp w)     ;; w = n3
					   (mul x 0)   ;; x = 0
					   (add x z)   ;; x = z  : z = (+ n2 (* 26 n1) 35)
					   (mod x 26)  ;; x = mod (+ n2 (* 26 n1) 35) 26
					   (div z 1)   ;; 
					   (add x 14)  ;; x = mod (+ n2 (* 26 n1) 35) 26
					   (eql x w)   ;; x =0 : always false
					   (eql x 0)   ;; x = 1 : x was 0
					   (mul y 0)   ;;             
					   (add y 25)  ;; y=  25
					   (mul y x)   ;; y = 25 
					   (add y 1)   ;; y = 26
					   (mul z y)   ;; z = 26 * (+ n2 (* 26 n1) 35)
					   (mul y 0)   ;; 
					   (add y w)   ;; y = n3
					   (add y 12)  ;; y = n3 + 12
					   (mul y x)   ;; y = n3 + 12
					   (add z y)   ;; z = n3 + 12 + 26 * (+ n2 (* 26 n1) 35)         ...........line 55
					   )
					 )))
				   (let ((wout (second (assq 'w out)))
					 (xout (second (assq 'x out)))
					 (yout (second (assq 'y out)))
					 (zout (second (assq 'z out))))
				     ;;(format #t "n1 = ~a : n2 = ~a : n3 = ~a : out = ~a ~% " n1 n2 n3 out)
				     (assert (= zout (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))
				     (assert (= xout 1))
				     (assert (= yout (+ n3 12)))
				     (assert (= wout n3))
				     ))))))))


;; w = n3
;; x = 1
;; y = (+ n3 12)
;; z = (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))
;;

(define (test-4)  
  (let ((vals '(1 2 3 4 5 6 7 8 9)))
    (do-list (n1 vals)
	     (do-list (n2 vals)
		      (do-list (n3 vals)
			       (do-list (n4 vals)				       
					(let ((vars `((w ,n3)
						      (x 1)
						      (y ,(+ n3 12))
						      (z ,(+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))))
					  (let ((out
						 (exec
						  vars
						  `(,n4)
						  '(
						    ;; lines 55 to 73
						    (inp w)     ;; w = n4 
						    (mul x 0)   ;; x = 0
						    (add x z)   ;; x = z : x = (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))
						    (mod x 26)  ;; x = (modulo (+ n3 12 (* 26 (+ n2 (* 26 n1) 35))) 26)
						    (div z 1)   ;;
						    (add x 13)  ;; x = (+ 13 (modulo (+ n3 12 (* 26 (+ n2 (* 26 n1) 35))) 26))
						    (eql x w)   ;; guess false since add 13 cant ever be tre: x = w ?? n4
						    (eql x 0)   ;; x = 1
						    (mul y 0)   ;; y = 0
						    (add y 25)  ;; y = 25
						    (mul y x)   ;; y = 25
						    (add y 1)   ;; y = 26
						    (mul z y)   ;; z = (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35))))
						    (mul y 0)   ;; y = 0
						    (add y w)   ;; y = n4
						    (add y 6)   ;; y = (+ 6 n4)
						    (mul y x)   ;; 
						    (add z y)   ;; z = (+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))
						    )   
						  )))
					    (let ((wout (second (assq 'w out)))
						  (xout (second (assq 'x out)))
						  (yout (second (assq 'y out)))
						  (zout (second (assq 'z out))))
					      ;;(format #t "n1 = ~a : n2 = ~a : n3 = ~a : out = ~a ~% " n1 n2 n3 out)
					      (assert (= zout (+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))))
					      (assert (= xout 1))
					      (assert (= yout (+ n4 6)))
					      (assert (= wout n4))
					      )))))))))



;; w x y z 
;; w = n4
;; x = 1
;; y = (+ n4 6)
;; z = (+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))
(define (test-5)  
  (let ((vals '(1 2 3 4 5 6 7 8 9)))
    (do-list (n1 vals)
	     (do-list (n2 vals)
		      (do-list (n3 vals)
			       (do-list (n4 vals)
					(do-list (n5 vals)				       						
						 (let ((vars `((w ,n4)
							       (x 1)
							       (y ,(+ n4 6))
							       (z ,(+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))))))
						   (let ((out
							  (exec
							   vars
							   `(,n5)
							   '(
							     ;; lines 73 to 91
							     (inp w)     ;; w = n5
							     (mul x 0)   ;; x = 0
							     (add x z)   ;; x = z'
							     (mod x 26)  ;; x = modulo z' 26
							     (div z 26)  ;; z = floor z' 26 -- difference
							     (add x -6)  ;; x = (- (modulo z' 26) 6)
							     (eql x w)   ;;
							     (eql x 0)   ;; x = 0 or x = 1
							     (mul y 0)   ;; y = 0
							     (add y 25)  ;; y = 25
							     (mul y x)   ;; y = 0 or y = 25    [x = 0 ; x = 1 ]
							     (add y 1)   ;; y = 1 or y = 26
							     (mul z y)   ;; z = floor z' 26 or  (* 26 (floor z' 26))
							     (mul y 0)   ;; y = 0
							     (add y w)   ;; y = n5
							     (add y 9)   ;; y = n5 + 9 
							     (mul y x)   ;; y = 0 or y = (n5 + 9) 
							     (add z y)   ;; z = (floor z' 26) or (+ n5 9 26 (floor z' 26))
							     )      
							   )))
						     (let ((wout (second (assq 'w out)))
							   (xout (second (assq 'x out)))
							   (yout (second (assq 'y out)))
							   (zout (second (assq 'z out)))
							   (z-prime (+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))))
						       
						        (assert (or (= zout (floor (/ z-prime 26)))
								   (= zout (+ n5 9 (* 26 (floor (/ z-prime 26)))))))						       
						       (assert (or (= xout 1)
								   (= xout 0)))
						       (assert (or (= yout 0)
								   (= yout (+ n5 9))))
						       (assert (= wout n5))
						       ))))))))))

#|
splits cause inability to solve question
n5 = (- (modulo z' 26) 6)

where z' is (+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35)))))
n1 , n2 , n3 , n4 , n5 = [ 1 .. 9 ] inclusive

|#


#|
suppose i try to solve the last input section
for n14 input

test 5   line  
test 6   line
test 7   line
test 8   line 
test 9   line 
test 10  line
test 11  line 
test 12  line 199
test 13  line 217 .....
test 14  line 235 ....

|#

(define (test-6)
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n6 vals)
	     (do-for z (0 5000 1) ;; (z (iota 1000))
		     (let ((vars `((w 0)
				   (x 0)
				   (y 0)
				   (z ,z))))
		       (let ((out
			      (exec
			       vars
			       `(,n6)
			       '(
				 ;; test 6
				 (inp w)      ;; w = n6
				 (mul x 0)    ;; x = 0
				 (add x z)    ;; x = z
				 (mod x 26)   ;; x = (mod z 26)
				 (div z 26)   ;; z = (div z 26)
				 (add x -14)  ;; x = (- (mod z 26) 14)
				 (eql x w)    ;; x = w ? 
				 (eql x 0)    ;; x = 0 or x = 1
				 (mul y 0)    ;; y = 0
				 (add y 25)   ;; y = 25
				 (mul y x)    ;; y = 0 or y = 25
				 (add y 1)    ;; y = 1 or y = 26
				 (mul z y)    ;; z = (div z 26) or z = (* 26 (div z 26))
				 (mul y 0)    ;; y = 0
				 (add y w)    ;; y = n6
				 (add y 15)   ;; y = (+ n6 15)
				 (mul y x)    ;; y = 0 or y = (+ n6 15)
				 (add z y)    ;; z = (div z 26) or (+ n6 15 (* 26 (div z 26)))
				 ))))
			 (let ((wout (second (assq 'w out)))
			       (xout (second (assq 'x out)))
			       (yout (second (assq 'y out)))
			       (zout (second (assq 'z out)))
			       (z-prime z))

			   ;; (let ((a (div z-prime 26))
			   ;; 	 (b (+ n6 15 (* 26 (div z-prime 26)))))
			   ;;   (format #t "z = ~a : zout = ~a : a = ~a : b = ~a ~%" z zout a b ))
			   
			   (assert (or (= zout (div z-prime 26))
				       (= zout (+ n6 15 (* 26 (div z-prime 26))))))
			   
			   (assert (or (= xout 1)
				       (= xout 0)))
			   
			   (assert (or (= yout 0)
				       (= yout (+ n6 15))))
			   
			   (assert (= wout n6))
			   )))))))







(define (test-7)
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n7 vals)
	     (do-for z (0 5000 1) ;; (z (iota 1000))
		     (let ((vars `((w 0)
				   (x 0)
				   (y 0)
				   (z ,z))))
		       (let ((out
			      (exec
			       vars
			       `(,n7)
			       '(
				 ;; test 7
				 (inp w)      ;; w = n7
				 (mul x 0)    ;; x = 0
				 (add x z)    ;; x = z'
				 (mod x 26)   ;; x = (mod z 26)
				 (div z 1)    ;; 
				 (add x 14)   ;; x = (+ 14 (mod z 26))
				 (eql x w)    ;; x = w ? 
				 (eql x 0)    ;; x = 0 or x = 1
				 (mul y 0)    ;; y = 0				  
				 (add y 25)   ;; y= 25
				 (mul y x)    ;; y = 0 or y = 25
				 (add y 1)    ;; y = 1 or y = 26
				 (mul z y)    ;; z = z' or z = (* 26 z')
				 (mul y 0)    ;; y = 0
				 (add y w)    ;; y = n7
				 (add y 7)    ;; y = (+ n7 7)
				 (mul y x)    ;; y = 0 or y = (+ n7 7)
				 (add z y)    ;; z = z'  or z = (+ n7 7 (* 26 z'))
				 ))))
			 (let ((wout (second (assq 'w out)))
			       (xout (second (assq 'x out)))
			       (yout (second (assq 'y out)))
			       (zout (second (assq 'z out)))
			       (z-prime z))

			   (assert (or (= zout z-prime)
				       (= zout (+ n7 7 (* 26 z-prime)))))
			   
			   (assert (or (= xout 1)
				       (= xout 0)))
			   
			   (assert (or (= yout 0)
				       (= yout (+ n7 7))))
			   
			   (assert (= wout n7))
			   )))))))


(define (test-8)
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n8 vals)
	     (do-for z (0 5000 1) ;; (z (iota 1000))
		     (let ((vars `((w 0)
				   (x 0)
				   (y 0)
				   (z ,z))))
		       (let ((out
			      (exec
			       vars
			       `(,n8)
			       '(
				 ;; test 8
				 (inp w)      ;; w = n8
				 (mul x 0)    ;; x = 0
				 (add x z)    ;; x = z' 
				 (mod x 26)   ;; x = (mod z' 26)
				 (div z 1)    ;; 
				 (add x 13)   ;; x = (+ 13 (mod z' 26))
				 (eql x w)    ;; x = w ?
				 (eql x 0)    ;; x = 0 or x = 1
				 (mul y 0)    ;; y = 0
				 (add y 25)   ;; y = 25
				 (mul y x)    ;; y = 0 or y = 25
				 (add y 1)    ;; y = 1 or y = 26
				 (mul z y)    ;; z = z'  or (* 26 z')
				 (mul y 0)    ;; y = 0
				 (add y w)    ;; y = n8
				 (add y 12)   ;; y = (+ n8 12)
				 (mul y x)    ;; y = 0  or y = (+ n8 12)
				 (add z y)    ;; z = z'  or z = (+ n8 12 (* 26 z'))

				 ))))
			 (let ((wout (second (assq 'w out)))
			       (xout (second (assq 'x out)))
			       (yout (second (assq 'y out)))
			       (zout (second (assq 'z out)))
			       (z-prime z))

			   (assert (or (= zout z-prime)
				       (= zout (+ n8 12 (* 26 z-prime)))))
			   
			   (assert (or (= xout 1)
				       (= xout 0)))
			   
			   (assert (or (= yout 0)
				       (= yout (+ n8 12))))
			   
			   (assert (= wout n8))
			   )))))))

  
(define (test-9)
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n9 vals)
	     (do-for z (0 5000 1) ;; (z (iota 1000))
		     (let ((vars `((w 0)
				   (x 0)
				   (y 0)
				   (z ,z))))
		       (let ((out
			      (exec
			       vars
			       `(,n9)
			       '(
				 ;; test 9
				 (inp w)      ;; w = n9
				 (mul x 0)    ;; x = 0
				 (add x z)    ;; x = 'z
				 (mod x 26)   ;; x = (mod z' 26)
				 (div z 26)   ;; z = (div z 26)
				 (add x -8)   ;; x = (- (mod z' 26) 8)
				 (eql x w)    ;; x =w ? 
				 (eql x 0)    ;; x = 0 or x = 1
				 (mul y 0)    ;; y = 0
				 (add y 25)   ;; y = 25
				 (mul y x)    ;; y = 0 or y = 25
				 (add y 1)    ;; y = 1 or y = 26
				 (mul z y)    ;; z = (div z 26) or z = (* 26 (div z 26))
				 (mul y 0)    ;; y =0
				 (add y w)    ;; y = n9 
				 (add y 15)   ;; y = (+ n9 15)
				 (mul y x)    ;; y = 0  or y =   (+ n9 15)
				 (add z y)    ;; z = (div z 26) or z = (+ n9 15 (* 26 (div z 26)))

				 ))))
			 (let ((wout (second (assq 'w out)))
			       (xout (second (assq 'x out)))
			       (yout (second (assq 'y out)))
			       (zout (second (assq 'z out)))
			       (z-prime z))

			   ;; (when (= zout 21332)
			   ;;   (format #t "solution for test-10 : n9 [~a] :: z-prime [~a] :: zout [~a] ~%" n9 z-prime zout))

			   ;;(format #t "test-9 : n9 [~a] :: z-prime [~a] :: zout [~a] ~%" n9 z-prime zout)
			   
			   ;;(format #t "~a ~%" zout)
			   (assert (or (= zout (div z-prime 26))
				       (= zout (+ n9 15 (* 26 (div z-prime 26))))))
			   
			   (assert (or (= xout 1)
				       (= xout 0)))
			   
			   (assert (or (= yout 0)
				       (= yout (+ n9 15))))
			   
			   (assert (= wout n9))
			   )))))))



(define (test-10)  
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n10 vals)
	     (do-for z (0 5000) ;; (z (iota 1000))
		       (let ((vars `((w 0)
				     (x 0)
				     (y 0)
				     (z ,z))))
			 (let ((out
				(exec
				 vars
				 `(,n10)
				 '(
				   ;; test 10
				   (inp w)      ;; w = n10
				   (mul x 0)    ;; x = 0
				   (add x z)    ;; x = z'
				   (mod x 26)   ;; x = (mod z' 26)
				   (div z 26)   ;; z = (div z' 26)
				   (add x -15)  ;; x = (- (mod z' 26) 15)
				   (eql x w)    ;; x = w ? 
				   (eql x 0)    ;; x = 0 or x = 1
				   (mul y 0)    ;; y = 0
				   (add y 25)   ;; y = 25
				   (mul y x)    ;; y = 0 or y = 25
				   (add y 1)    ;; y = 1 or y = 26
				   (mul z y)    ;; z = (div z' 26) or (* 26 (div z' 26))
				   (mul y 0)    ;; y = 0
				   (add y w)    ;; y = n10
				   (add y 3)    ;; y = (+ 3 n10)
				   (mul y x)    ;; y = 0 or y = (+ 3 n10)
				   (add z y)    ;; z = (div z' 26) or z = (+ 3 n10 (* 26 (div z' 26)))
				   ))))
			   (let ((wout (second (assq 'w out)))
				 (xout (second (assq 'x out)))
				 (yout (second (assq 'y out)))
				 (zout (second (assq 'z out)))
				 (z-prime z))

			     ;; (when (= zout 21332)
			     ;;   (format #t "solution for test-10 : n10 [~a] :: z-prime [~a] :: zout [~a] ~%" n10 z-prime zout))

			     ;;(format #t "test-11 : n10 [~a] :: z-prime [~a] :: zout [~a] ~%" n10 z-prime zout)
			     
			     ;;(format #t "~a ~%" zout)
			     (assert (or (= zout (div z-prime 26))
				      (= zout (+ 3 n10 (* 26 (div z-prime 26))))))
			     
			     (assert (or (= xout 1)
					 (= xout 0)))
			     
			     (assert (or (= yout 0)
					 (= yout (+ n10 3))))
			     
			     (assert (= wout n10))
			     )))))))



#|
test-10 : condition to be true
z' = 16 + k * 26

+/- 16
+/- 42
+/0 68
+/- 94
etc....

|#









(define (test-11)  
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 817))
    (do-list (n11 vals)
	     (do-for z (0 5000) 
		       (let ((vars `((w 0)
				     (x 0)
				     (y 0)
				     (z ,z))))
			 (let ((out
				(exec
				 vars
				 `(,n11)
				 '(
				   ;; test 11
				    (inp w)     ;; w = n11
				    (mul x 0)   ;; x = 0
				    (add x z)   ;; x = z'
				    (mod x 26)  ;; x = (mod z' 26)
				    (div z 1)   ;; z'
				    (add x 10)  ;; x = (+ 10 (mod z' 26))
				    (eql x w)   ;; x = 0 : x = w ? FALSE always as adding 10 always greater than 
				    (eql x 0)   ;; x = 1
				    (mul y 0)   ;; y = 0
				    (add y 25)  ;; y = 25
				    (mul y x)   ;; y = 25
				    (add y 1)   ;; y = 26
				    (mul z y)   ;; z = (* 26 z')
				    (mul y 0)   ;; y = 0
				    (add y w)   ;; y = n11
				    (add y 6)   ;; y = (+ 6 n11)
				    (mul y x)   ;; y = (+ 6 n11)
				    (add z y)	;; z = (+ 6 n11  (* 26 z'))		          
				 ))))
			   (let ((wout (second (assq 'w out)))
				 (xout (second (assq 'x out)))
				 (yout (second (assq 'y out)))
				 (zout (second (assq 'z out)))
				 (z-prime z))

			     ;; test-12 likes z-prime as 21332
			     (when (= zout 21332)
			       (format #t "solution for test-11 : n11 [~a] :: z-prime [~a] :: zout [~a] ~%" n11 z-prime zout))
			     
			     ;;(format #t "~a ~%" zout)
			     (assert (or ;;(= zout z-prime )
					 (= zout (+ 6 n11 (* 26 z-prime)))))		          
			     
			     (assert (or (= xout 1)
					 (= xout 0)))
			     
			     (assert (or (= yout 0)
					 (= yout (+ n11 6))))
			     
			     (assert (= wout n11))
			     )))))))




#|
z = (+ 6 n11 (* 26 z'))  == 21332
z = (+ n11 (* 26 z'))  == 21326
z = (* 26 z')  == 21317 ... 21325
       z' = 
#;1529> (do-for z (21317 21326) (format #t "z = ~a : ~a ~%" z (/ z 26)))
z = 21317 : 21317/26 
z = 21318 : 10659/13 
z = 21319 : 21319/26 
z = 21320 : 820 
z = 21321 : 21321/26 
z = 21322 : 10661/13 
z = 21323 : 21323/26 
z = 21324 : 10662/13 
z = 21325 : 21325/26 




|#

  




(define (test-12)  
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n12 vals)
	     (do-for z (0 5000)
		       (let ((vars `((w 0)
				     (x 0)
				     (y 0)
				     (z ,z))))
			 (let ((out
				(exec
				 vars
				 `(,n12)
				 '(
				   ;; test 12
				   (inp w)     ;;  w = n12
				   (mul x 0)   ;;  x = 0
				   (add x z)   ;;  x = z'
				   (mod x 26)  ;;  x = mod z' 26
				   (div z 26)  ;;  z = div z' 26
				   (add x -11) ;;  x = (- (mod z' 26) 11)
				   (eql x w)   ;;  x = w ? 
				   (eql x 0)   ;;  x = 0 or x = 1
				   (mul y 0)   ;;  y = 0
				   (add y 25)  ;;  y = 25
				   (mul y x)   ;;  y = 0 or y = 25
				   (add y 1)   ;;  y = 1 or y = 26
				   (mul z y)   ;;  z = div z' 26 or (* 26 (div z' 26))
				   (mul y 0)   ;;  y = 0
				   (add y w)   ;;  y = n12
				   (add y 2)   ;;  y = (+ n12 2)
				   (mul y x)   ;;  y = 0 or y = (+ n12 2)
				   (add z y)   ;;  z = (div z' 26)  or (+ n12 2 (* 26 (div z' 26)))
				   )            
				 )))
			   (let ((wout (second (assq 'w out)))
				 (xout (second (assq 'x out)))
				 (yout (second (assq 'y out)))
				 (zout (second (assq 'z out)))
				 (z-prime z))

			     ;; test-13 likes z-prime as 820
			     (when (= zout 820)
			       (format #t "solution for test-12 : n13 [~a] :: z-prime [~a] :: zout [~a] ~%" n12 z-prime zout))
			     
			     ;;(format #t "~a ~%" zout)
			     (assert (or (= zout (div z-prime 26))
					 (= zout (+ n12 2 (* 26 (div z-prime 26))))))
			     
			     (assert (or (= xout 1)
					 (= xout 0)))
			     
			     (assert (or (= yout 0)
					 (= yout (+ n12 2))))
			     
			     (assert (= wout n12))
			     )))))))






(define (test-13)  
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n13 vals)
	     (do-for z (0 5000) 
		       (let ((vars `((w 0)
				     (x 0)
				     (y 0)
				     (z ,z))))
			 (let ((out
				(exec
				 vars
				 `(,n13)
				 '(
				   ;; lines 217 to 235
				   (inp w)      ;;  w = n13
				   (mul x 0)    ;;  x = 0
				   (add x z)    ;;  x = z;
				   (mod x 26)   ;; x = (mod z' 26)
				   (div z 26)   ;; z = (div z' 26)
				   (add x -13)  ;; x = (- (mod z' 26) 13)
				   (eql x w)    ;; x = w ?
				   (eql x 0)    ;; x = 0 or 1
				   (mul y 0)    ;; y = 0
				   (add y 25)   ;; y = 25
				   (mul y x)    ;; y = 0 or y = 25
				   (add y 1)    ;; y = 1 or y = 26
				   (mul z y)    ;; z = (div z' 26) or (* 26 (div z' 26))
				   (mul y 0)    ;; y = 0
				   (add y w)    ;; y = n13
				   (add y 10)   ;; y = (+ n13 10)
				   (mul y x)    ;; y = 0 or y = (+ n13 10)
				   (add z y)    ;; z = (div z' 26) or z = (+ n13 10 (* 26 (div z' 26)))
				   )         
				 )))
			   (let ((wout (second (assq 'w out)))
				 (xout (second (assq 'x out)))
				 (yout (second (assq 'y out)))
				 (zout (second (assq 'z out)))
				 (z-prime z))

			     ;; only solution from test-14 was z-prime to be 31
			     (when (= zout 31)
			       (format #t "solution for test-13 : n13 [~a] :: z-prime [~a] :: zout [~a] ~%" n13 z-prime zout))
			     
			     ;;(format #t "~a ~%" zout)
			     (assert (or (= zout (floor (/ z-prime 26)))
					 (= zout (+ n13 10 (* 26 (floor (/ z-prime 26)))))))
			     
			     (assert (or (= xout 1)
					 (= xout 0)))
			     (assert (or (= yout 0)
					 (= yout (+ n13 10))))
			     (assert (= wout n13))
			     )))))))








(define (test-14)  
  (let ((vals '(1 2 3 4 5 6 7 8 9))
	(z 0))
    (do-list (n14 vals)
	     (do-for z (0 5000)
		      (let ((vars `((w 0)
				    (x 0)
				    (y 0)
				    (z ,z))))
			(let ((out
			       (exec
				vars
				`(,n14)
				'(
				  ;; lines 235 to 253
				  (inp w)     ;;  w = n14
				  (mul x 0)   ;;  x = 0
				  (add x z)   ;;  x = z'
				  (mod x 26)  ;;  x = (modulo z' 26)
				  (div z 26)  ;;  z = (div z' 26)
				  (add x -4)  ;;  x = (- (modulo z' 26) 4)
				  (eql x w)   ;;  x = n4 ? 
				  (eql x 0)   ;;  x = 0 or 1
				  (mul y 0)   ;;  y = 0
				  (add y 25)  ;;  y = 25
				  (mul y x)   ;;  y = 0 or 25
				  (add y 1)   ;;  y = 1 or 26
				  (mul z y)   ;;  z = (div z' 26) or (* 26 (div z' 26))
				  (mul y 0)   ;;  y = 0
				  (add y w)   ;;  y = n14
				  (add y 12)  ;;  y = n14 + 12
				  (mul y x)   ;;  y = 0  or n14 + 12
				  (add z y)   ;;  z =  (div z' 26)  or  (+ n14 12 (* 26 (div z' 26)))
 				  )         
				)))
			  (let ((wout (second (assq 'w out)))
				(xout (second (assq 'x out)))
				(yout (second (assq 'y out)))
				(zout (second (assq 'z out)))
				(z-prime z))

			    (when (= zout 1)
			      (format #t "solution for test-14 : n14 [~a] :: z-prime [~a] :: zout [~a] ~%" n14 z-prime zout))
			    
			    ;;(format #t "~a ~%" zout)
			    (assert (or (= zout (floor (/ z-prime 26)))
					(= zout (+ n14 12 (* 26 (floor (/ z-prime 26)))))))
			    
			    (assert (or (= xout 1)
					(= xout 0)))
			    (assert (or (= yout 0)
					(= yout (+ n14 12))))
			    (assert (= wout n14))
			    )))))))













(define (test-con-5)
  (let ((yes 0)(no 0))
    (let ((seq19 (cdr (iota 10))))
      (do-list (n1 seq19)
	       (do-list (n2 seq19)		      
			(do-list (n3 seq19)
				 (do-list (n4 seq19)
					  (do-list (n5 seq19)
						   (let* ((z-prime (+ 6 n4 (* 26 (+ n3 12 (* 26 (+ n2 (* 26 n1) 35))))))
							  (x (- (modulo z-prime 26) 6))
							  (w n5))
						     (cond
						      ((= x w) (set! yes (+ yes 1)))
						      (#t (set! no (+ no 1)))))))))))
    (list 'yes yes 'no no)))










(define (test-con-3)
  (let ((seq19 (cdr (iota 10))))
    (do-list (n1 seq19)
	     (do-list (n2 seq19)		      
		      (do-list (n3 seq19)
			       (let ((x (modulo (+ n2 (* 26 n1) 35) 26))
				     (w n1))
				 (format #t "n1 = ~a : n2 = ~a : n3 = ~a : x = w ??? ~a ~%" n1 n2 n3 (= x w))))))))



				






#|
assume random input data w x y z
inputs used z
n1 from data
|#
(define (test-1f)
  (let ((seq19 (cdr (iota 10))))
    (do-list (z seq19)
	     (do-list (n1 seq19)
		      (let ((x (+ 10 (modulo z 26)))
			    (w n1))
			(format #t "n1 = ~a : z = ~a : ?? ~a ~%" n1 z (= x w) ))))))

(define (test-eql)
  (let ((seq19 (cdr (iota 10))))
    (let ((zin 0)(win 0))
      (do-list (xin seq19)
	       (do-list (yin seq19)
			(let ((out (exec `((x ,xin)(y ,yin)(z ,zin)(w ,win))
					 '()
					 `(
					   (eql x y)
					   ))))
			  (let ((wout (second (assq 'w out)))
				(xout (second (assq 'x out)))
				(yout (second (assq 'y out)))
				(zout (second (assq 'z out))))
			    (format #t "x = ~a : y = ~a : eql x y = ~a ~%" xin yin out)
			    (cond
			     ((= xin yin) (assert (= xout 1)))
			     (#t (assert (= xout 0)))))))))))

  


(define (test-1-add)
  (let ((seq19 (cdr (iota 10))))
    (let ((zin 0)(win 0))
      (do-list (xin seq19)
	       (do-list (yin seq19)
			(let ((out (exec `((x ,xin)(y ,yin)(z ,zin)(w ,win))
					 '()
					 `(
					   (add z x)
					   (add z y)
					   ))))
			  (let ((wout (second (assq 'w out)))
				(xout (second (assq 'x out)))
				(yout (second (assq 'y out)))
				(zout (second (assq 'z out))))
			    (format #t "x = ~a : y = ~a : add x y = ~a ~%" xin yin out)
			    (assert (= zout (+ xin yin))))))))))















#|
this is ok as an approch
but only 3 variables n1 n2 n3 in ....
what need is a way to reduce the program to something simpler

(mul x 0) is that just setting x to zero ?

|#


(define (test-mul-zero)  
  (let ((vals  (cdr (iota 100))))
    (do-list (xin vals)
	     (let* ((vars `((w 0)(x ,xin)(y 0)(z 0)))
		    (data '())
		    (prog '((mul x 0)))
		    (out (exec vars data prog)))
	       (let ((wout (second (assq 'w out)))
		     (xout (second (assq 'x out)))
		     (yout (second (assq 'y out)))
		     (zout (second (assq 'z out))))
		 (format #t "x in ~a : x out ~a ~%" xin xout))))))


(define (test-div-one)  
  (let ((vals  (cdr (iota 100))))
    (do-list (zin vals)
	     (let* ((vars `((w 0)(x 0)(y 0)(z ,zin)))
		    (data '())
		    (prog '((div z 1)))
		    (out (exec vars data prog)))
	       (let ((wout (second (assq 'w out)))
		     (xout (second (assq 'x out)))
		     (yout (second (assq 'y out)))
		     (zout (second (assq 'z out))))
		 (format #t "x in ~a : x out ~a ~%" zin zout))))))




	       















	














