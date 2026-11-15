
;; chicken 6.1 scheme code
;; read file from ../input.txt and lets see it

(import scheme (chicken io))
(import (chicken format))
(import (chicken process-context))

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
(define (convert str)
  (convert2 (string->list str)))

(define (convert2 xs)
  (cond 
   ((null? xs) '())
   (#t (let ((hd (car xs))
             (tl (cdr xs))
             (tmp '()))
         (cond
          ((equal? 
         
         (append 
          (convert tl))))))
