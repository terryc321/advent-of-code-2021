
;; chicken 6.1 scheme code
;; read file from ../input.txt and lets see it

(import scheme (chicken io))
(import (chicken format))
(import (chicken process-context))
(import srfi-1)

;; weird geiser setup wants to jump to top of git tree ?
;; (format #t "current directory is ~a~%" (current-directory))
;; (change-directory "day16/chicken")
;; (format #t "current directory is ~a~%" (current-directory))

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

;; repeat something n times 
;; pros : no macrology required
;; cons : wrap body in another lambda 
(define repeat (lambda (n fn)
                 (cond
                  ((> n 0) (fn n) (repeat (- n 1) fn))
                  (#t #f))))


;; (string-length "110100101111111000101000")
;; 110100101111111000101000
;; VVVTTTxAAAAxBBBBxCCCC
;; 6 +  3x5 (15) or (21) should be a multiple of 24 so drop 3


(define bin2dec 
  (lambda (xs)
    (letrec ((bin (lambda (ys pwr acc)
                    (cond
                     ((null? ys) acc)
                     (#t (let* ((hd (car ys)))
                           (cond
                            ((= hd 1) (let ((extra (expt 2 pwr)))
                                        ;;(format #t "adding ~a~%~" extra)
                                        (bin (cdr ys) (+ pwr 1) (+ acc extra))))
                            (#t (bin (cdr ys) (+ pwr 1) acc)))))))))
      (let ((result (bin (reverse xs) 0 0)))
        ;;(format #t "bool conversion ~a => ~a ~%" xs result)
        result))))

;; while macro
(define-syntax while
  (syntax-rules ()
    ((_ condition body ...) 
     (letrec ((foo (lambda ()
                     (when condition body ...
                           (foo)))))
       (foo)))))

(define (test-while)
  (let ((a 1))
    (while (<= a 10) (format #t "a = ~a~%" a) (set! a (+ a 1)))))



(define make-pack 
  (lambda (xs)
    (let ((total-version 0))
    (letrec ((pack (lambda ()
                     (format #t ".... new packet ... ~%" )
                     ;; pack-lit v v v t t t 1 a a a 1 b b b 0 c c c ? ? ? ? 
                     ;;          0 1 2 3 4 5 
                     (letrec ((pack-lit (lambda ()  
                                          (format #t "pack-literal ~%")
                                          (let ((bools '())
                                                (nbits 6)
                                                (orig (length xs)))
                                            (format #t "original length ~a ~%" orig)
                                            (set! xs (drop xs 6)) ;; drop vvv ttt  
                                            (letrec ((keep (lambda ()
                                                             (let ((again (list-ref xs 0))
                                                                   (a (list-ref xs 1))
                                                                   (b (list-ref xs 2))
                                                                   (c (list-ref xs 3))
                                                                   (d (list-ref xs 4)))
                                                               (format #t "bits = ~a ~a ~a ~a~%" a b c d)
                                                               (set! nbits (+ nbits 5))
                                                               (set! bools (append bools (list a b c d)))
                                                               (cond
                                                                ((= again 1)  ;; loop again
                                                                 (set! xs (drop xs 5)) 
                                                                 (keep))
                                                                (#t (set! xs (drop xs 5))
                                                                    ;; how many to drop? 
                                                                    (let* ((orig-after (length xs))
                                                                           (orig-len (- orig orig-after)))
                                                                      (format #t "original before ~a : after ~a : diff ~a~%" 
                                                                              orig orig-after orig-len)
                                                                      ;; drop bits such literal packet not multiple of 4 
                                                                      (letrec ((mult4 (lambda ()
                                                                                        (cond
                                                                                         ((not (zero? (modulo orig-len 4)))
                                                                                          (format #t "dropping a bit~%")
                                                                                          (set! xs (drop xs 1))
                                                                                          (set! orig-len (+ 1 orig-len))
                                                                                          (mult4))))))
                                                                        ;;(mult4) 
                                                                        ;;(set! bools (reverse bools))
                                                                        (let ((found (bin2dec bools)))
                                                                          (format #t "number is ~A~%" found))))))))))
                                              (keep))))
                                        ) ;; pack-lit                            
                              ;; pack-15 v v v t t t L 1 2 3 4  5  6  7  8  9  10 11 12 13 14 15    
                              ;;         0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 ... nth list ref offsets
                              ;;  (7+15 => 22) 22 bool consumed
                              (pack-15 (lambda ()                         
                                         ;; (format #t "pack-11 : before : length xs => ~a ~%" (length xs))
                                         (set! xs (drop xs 7))         
                                         ;; (format #t "pack-11 : after : length xs => ~a ~%" (length xs))
                                         (let* ((len (bin2dec (take xs 15))))
                                           (format #t "pack-15 : len in bits ~a : (15 bits) ~a ~%" len (take xs 15))
                                           (set! xs (drop xs 15))
                                           (let* ((here (length xs))
                                                  (target (- here len)))
                                             ;; keep calling pack until we have exhausted this packet
                                             (while (> (length xs) target )
                                               (pack)))))) ;; pack15

                              ;; pack-11 v v v t t t L 1 2 3 4  5  6  7  8  9  10 11      
                              ;;         0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17  ... nth list ref offsets  
                              ;; (7+11 => 18)   18 bool consumed
                              (pack-11 (lambda ()   
                                         ;; shortcut for writing format ~a ~a ~% 
                                         ;; (format #t "pack-11 : before : length xs => ~a ~%" (length xs))
                                         (set! xs (drop xs 7))         
                                         ;; (format #t "pack-11 : after drop 7 : length xs => ~a ~%" (length xs))
                                         (let* ((number (bin2dec (take xs 11))))
                                           (format #t "pack-11 : number contained ~a : (11 bits) ~a~%" number (take xs 11))
                                           (set! xs (drop xs 11))
                                           ;; keep calling pack until we have exhausted this packet
                                           (repeat number (lambda (n) (pack))))))
                              );;letrec
                       (let* ((version (+ (* 4 (list-ref xs 0))
                                          (* 2 (list-ref xs 1))
                                          (* 1 (list-ref xs 2))))
                              (type    (+ (* 4 (list-ref xs 3))
                                          (* 2 (list-ref xs 4))
                                          (* 1 (list-ref xs 5)))))
                         (format #t "version ~a : type ~a : (6 bits) ~a : (1 bit) ~a ~%" 
                                 version type (take xs 6) (take (drop xs 6) 1))
                         (set! total-version (+ version total-version))
                         (format #t "total-version so far= ~a~%" total-version)

                         (cond
                          ((= type 4) (pack-lit))
                          (#t (let ((len (list-ref xs 6)))
                                (cond 
                                 ((= len 0) (pack-15))
                                 ((= len 1) (pack-11))
                                 (#t (error "type len expected 0 or 1")))))))))))
      pack))))



;; ;; construct our own packet reader from the given input
;; (define demo 
;;   (lambda ()
;;     (let ((pack (make-pack (convert input))))
;;       (pack))))
;; 
;; (define example1 
;;   (lambda ()
;;     (let ((pack (make-pack (convert "D2FE28"))))
;;       (pack))))

;; explain-able systems , utilising gui components to explore data structures , 
;; 8A004A801A8002F478 represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.

(define-syntax make-run 
  (syntax-rules () 
    ((_ name code)
     (define name
       (lambda ()
         (format #t "running= ~a~%" code)
         (let ((pack (make-pack (convert code))))
           (pack)))))))

;; (define example2 
;;   (lambda ()
;;     (let ((pack (make-pack (convert "8A004A801A8002F478"))))
;;       (pack))))
(make-run demo input)
(make-run example1 "D2FE28")
(make-run example2 "38006F45291200")
(make-run example3 "8A004A801A8002F478")
(make-run example4 "620080001611562C8802118E34")
(make-run example5 "C0015000016115A2E0802F182340")
(make-run example6 "A0016C880162017C3686B18A3D4780")

;; 
;; represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.

;; C0015000016115A2E0802F182340 has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.

;; A0016C880162017C3686B18A3D4780 is an operator packet that contains an operator packet that contains an operator packet that contains five literal values; it has a version sum of 31.

;; ;; debugging "38006F45291200" 
;; (list->string (map (lambda (x) (if (= x 1) #\1 #\0)) (convert "38006F45291200")))
;; 00111000000000000110111101000101001010010001001000000000
;; 00111000000000000110111101000101001010010001001000000000
;; VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB


;; debugging example 4 saying 8 when target is 12 

;; debugging "620080001611562C8802118E34"
;; (list->string (map (lambda (x) (if (= x 1) #\1 #\0)) (convert "620080001611562C8802118E34")))
;; 01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100

(define (decon)
  (list->string (map (lambda (x) (if (= x 1) #\1 #\0)) (convert "620080001611562C8802118E34"))))

(define (part1)
  (demo))

#|

wording very mis-leading in 

.... new packet ... 
version 0 : type 4 : (6 bits) (0 0 0 1 0 0) : (1 bit) (0) 
total-version so far= 979  ................................... 979 .... suggested answer
pack-literal 
original length 18 
bits = 1 1 0 0
original before 18 : after 7 : diff 11
number is 12
#f
|#
