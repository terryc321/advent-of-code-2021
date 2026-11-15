
;; chicken 6.1 scheme code
;; read file from ../input.txt and lets see it

(import scheme (chicken io))
(import (chicken format))
(import (chicken process-context))
(import srfi-1)

;; weird geiser setup wants to jump to top of git tree ?
(format #t "current directory is ~a~%" (current-directory))
;;(change-directory "day16/chicken")

;; luckily only one input
(define (process-file filename)
  (call-with-input-file filename
    (lambda (port) 
      (read-line port))))

      ;; (let loop ((line (read-line port)))
      ;;   (unless (eof-object? line)
      ;;     ;; Do something with `line` here
      ;;     (print line)
      ;;     (format #t "the line is ~a chars long ~%" (string-length line))
      ;;     (loop (read-line port)))))))

;; Usage:
(define input (process-file "../input.txt"))

;; convert input into a series of binary 1s and 0s 
;; the conversion of hex letter into binary 

(define convert
  (lambda (str)
    (letrec ((convert2 (lambda (xs)
                         (cond 
                          ((null? xs) '())
                          (#t (let ((hd (car xs))
                                    (tl (cdr xs))
                                    (tmp '()))
                                (cond
                                 ((equal? hd #\0) (set! tmp '(0 0 0 0)))
                                 ((equal? hd #\1) (set! tmp '(0 0 0 1)))
                                 ((equal? hd #\2) (set! tmp '(0 0 1 0)))
                                 ((equal? hd #\3) (set! tmp '(0 0 1 1)))
                                 ((equal? hd #\4) (set! tmp '(0 1 0 0)))
                                 ((equal? hd #\5) (set! tmp '(0 1 0 1)))
                                 ((equal? hd #\6) (set! tmp '(0 1 1 0)))
                                 ((equal? hd #\7) (set! tmp '(0 1 1 1)))
                                 ((equal? hd #\8) (set! tmp '(1 0 0 0)))
                                 ((equal? hd #\9) (set! tmp '(1 0 0 1)))
                                 ((equal? hd #\A) (set! tmp '(1 0 1 0)))
                                 ((equal? hd #\B) (set! tmp '(1 0 1 1)))
                                 ((equal? hd #\C) (set! tmp '(1 1 0 0)))
                                 ((equal? hd #\D) (set! tmp '(1 1 0 1)))
                                 ((equal? hd #\E) (set! tmp '(1 1 1 0)))
                                 ((equal? hd #\F) (set! tmp '(1 1 1 1)))
                                 (#t (error "convert failed")))
                                (append tmp
                                        (convert2 tl))))))))
      (convert2 (string->list str)))))


;; (convert input) ... bools 11100010
(format #t "loaded convert...~%")

;; (convert "D2FE28")
;; (1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0)
;;  1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0  ... check against puzzle 

(define xs (convert input)) ;; a llist of bools

(define reset! (lambda () (set! xs (convert input))))


  


;; (define-syntax repeat
;;   (syntax-rules ()
;;     ((repeat n body) (letrec ((foo n)cond 
;;                       ((>= n 1) (begin body (repeat (- n 1) body)))
;;                       (else #f)))))

;; (define-syntax repeat
;;   (syntax-rules ()
;;     ((_ n body) (letrec ((foo (lambda (n) 
;;                                 (when (> n 0) body 
;;                                       (foo (- n 1))))))
;;                   (foo n)))))

(define repeat (lambda (n fn)
                 (cond
                  ((> n 0) (fn) (repeat (- n 1) fn))
                  (#t #f))))




(define pack 
  (lambda ()
    (letrec ((pack-lit (lambda ()  
                        (format #t "pack-literal ~%")
                               ))
             ;; pack-15 v v v t t t L 1 2 3 4  5  6  7  8  9  10 11 12 13 14 15    
             ;;         0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 ... nth list ref offsets
             ;;  (7+15 => 22) 22 bool consumed
             (pack-15 (lambda ()                         
                        (let* ((len (+ 
                                     (* (expt 2 0) (list-ref xs 21));15
                                     (* (expt 2 1) (list-ref xs 20));14
                                     (* (expt 2 2) (list-ref xs 19));13
                                     (* (expt 2 3) (list-ref xs 18));12
                                     (* (expt 2 4) (list-ref xs 17));11
                                     (* (expt 2 5) (list-ref xs 16));10
                                     (* (expt 2 6) (list-ref xs 15));9
                                     (* (expt 2 7) (list-ref xs 14));8
                                     (* (expt 2 8) (list-ref xs 13));7
                                     (* (expt 2 9) (list-ref xs 12));6
                                     (* (expt 2 10) (list-ref xs 11));5
                                     (* (expt 2 11) (list-ref xs 10));4
                                     (* (expt 2 12) (list-ref xs 9));3
                                     (* (expt 2 13) (list-ref xs 8));2
                                     (* (expt 2 14) (list-ref xs 7));1
                                     )))
                          (format #t "pack-15 : len ~a~%" len)
                          (set! xs (drop xs 22))
                          ;; do this len times 
                          (repeat len (lambda () (pack)))))
                      );;pack-15
             ;; pack-11 v v v t t t L 1 2 3 4  5  6  7  8  9  10 11      
             ;;         0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17  ... nth list ref offsets  
             ;; (7+11 => 18)   18 bool consumed
             (pack-11 (lambda ()                         
                        (let* ((len (+ 
                                     (* (expt 2 0) (list-ref xs 17));11
                                     (* (expt 2 1) (list-ref xs 16));10
                                     (* (expt 2 2) (list-ref xs 15));9
                                     (* (expt 2 3) (list-ref xs 14));8
                                     (* (expt 2 4) (list-ref xs 13));7
                                     (* (expt 2 5) (list-ref xs 12));6
                                     (* (expt 2 6) (list-ref xs 11));5
                                     (* (expt 2 7) (list-ref xs 10));4
                                     (* (expt 2 8) (list-ref xs 9));3
                                     (* (expt 2 9) (list-ref xs 8));2
                                     (* (expt 2 10) (list-ref xs 7));;1
                                     )))
                          (format #t "pack-11 : len ~a~%" len)
                          (set! xs (drop xs 18))
                          ;; do this len times 
                          (repeat len (lambda () (pack)))))
                      );;pack-11
             );;letrec
      (let* ((version (+ (* 4 (list-ref xs 0))
                         (* 2 (list-ref xs 1))
                         (* 1 (list-ref xs 2))))
             (type    (+ (* 4 (list-ref xs 3))
                         (* 2 (list-ref xs 4))
                         (* 1 (list-ref xs 5)))))
        (cond
         ((= type 4) (pack-lit))
         (#t (let ((len (list-ref xs 6)))
               (cond 
                ((= len 0) (pack-15))
                ((= len 1) (pack-11))
                (#t (error "type len expected 0 or 1"))))))))))


