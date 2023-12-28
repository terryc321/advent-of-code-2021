;;(import (lsp-server))

(import scheme)
(import (chicken process-context))
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
(change-directory "day15")
|#

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))


(define example 
  #(
    #(1 1 6 3 7 5 1 7 4 2)
    #(1 3 8 1 3 7 3 6 7 2)
    #(2 1 3 6 5 1 1 3 2 8)
    #(3 6 9 4 9 3 1 5 6 9)
    #(7 4 6 3 4 1 7 1 1 1)
    #(1 3 1 9 1 2 8 1 3 7)
    #(1 3 5 9 9 1 2 4 2 1)
    #(3 1 2 5 4 2 1 6 3 9)
    #(1 2 9 3 1 3 8 5 2 1)
    #(2 3 1 1 9 4 4 5 8 1)
    ))

(define example2
  #(
    #(1 1 6 3 7 5 1 7 4 2 2 2 7 4 8 6 2 8 5 3 3 3 8 5 9 7 3 9 6 4 4 4 9 6 1 8 4 1 7 5 5 5 1 7 2 9 5 2 8 6)
    #(1 3 8 1 3 7 3 6 7 2 2 4 9 2 4 8 4 7 8 3 3 5 1 3 5 9 5 8 9 4 4 6 2 4 6 1 6 9 1 5 5 7 3 5 7 2 7 1 2 6)
    #(2 1 3 6 5 1 1 3 2 8 3 2 4 7 6 2 2 4 3 9 4 3 5 8 7 3 3 5 4 1 5 4 6 9 8 4 4 6 5 2 6 5 7 1 9 5 5 7 6 3)
    #(3 6 9 4 9 3 1 5 6 9 4 7 1 5 1 4 2 6 7 1 5 8 2 6 2 5 3 7 8 2 6 9 3 7 3 6 4 8 9 3 7 1 4 8 4 7 5 9 1 4)
    #(7 4 6 3 4 1 7 1 1 1 8 5 7 4 5 2 8 2 2 2 9 6 8 5 6 3 9 3 3 3 1 7 9 6 7 4 1 4 4 4 2 8 1 7 8 5 2 5 5 5)
    #(1 3 1 9 1 2 8 1 3 7 2 4 2 1 2 3 9 2 4 8 3 5 3 2 3 4 1 3 5 9 4 6 4 3 4 5 2 4 6 1 5 7 5 4 5 6 3 5 7 2)
    #(1 3 5 9 9 1 2 4 2 1 2 4 6 1 1 2 3 5 3 2 3 5 7 2 2 3 4 6 4 3 4 6 8 3 3 4 5 7 5 4 5 7 9 4 4 5 6 8 6 5)
    #(3 1 2 5 4 2 1 6 3 9 4 2 3 6 5 3 2 7 4 1 5 3 4 7 6 4 3 8 5 2 6 4 5 8 7 5 4 9 6 3 7 5 6 9 8 6 5 1 7 4)
    #(1 2 9 3 1 3 8 5 2 1 2 3 1 4 2 4 9 6 3 2 3 4 2 5 3 5 1 7 4 3 4 5 3 6 4 6 2 8 5 4 5 6 4 7 5 7 3 9 6 5)
    #(2 3 1 1 9 4 4 5 8 1 3 4 2 2 1 5 5 6 9 2 4 5 3 3 2 6 6 7 1 3 5 6 4 4 3 7 7 8 2 4 6 7 5 5 4 8 8 9 3 5)
    #(2 2 7 4 8 6 2 8 5 3 3 3 8 5 9 7 3 9 6 4 4 4 9 6 1 8 4 1 7 5 5 5 1 7 2 9 5 2 8 6 6 6 2 8 3 1 6 3 9 7)
    #(2 4 9 2 4 8 4 7 8 3 3 5 1 3 5 9 5 8 9 4 4 6 2 4 6 1 6 9 1 5 5 7 3 5 7 2 7 1 2 6 6 8 4 6 8 3 8 2 3 7)
    #(3 2 4 7 6 2 2 4 3 9 4 3 5 8 7 3 3 5 4 1 5 4 6 9 8 4 4 6 5 2 6 5 7 1 9 5 5 7 6 3 7 6 8 2 1 6 6 8 7 4)
    #(4 7 1 5 1 4 2 6 7 1 5 8 2 6 2 5 3 7 8 2 6 9 3 7 3 6 4 8 9 3 7 1 4 8 4 7 5 9 1 4 8 2 5 9 5 8 6 1 2 5)
    #(8 5 7 4 5 2 8 2 2 2 9 6 8 5 6 3 9 3 3 3 1 7 9 6 7 4 1 4 4 4 2 8 1 7 8 5 2 5 5 5 3 9 2 8 9 6 3 6 6 6)
    #(2 4 2 1 2 3 9 2 4 8 3 5 3 2 3 4 1 3 5 9 4 6 4 3 4 5 2 4 6 1 5 7 5 4 5 6 3 5 7 2 6 8 6 5 6 7 4 6 8 3)
    #(2 4 6 1 1 2 3 5 3 2 3 5 7 2 2 3 4 6 4 3 4 6 8 3 3 4 5 7 5 4 5 7 9 4 4 5 6 8 6 5 6 8 1 5 5 6 7 9 7 6)
    #(4 2 3 6 5 3 2 7 4 1 5 3 4 7 6 4 3 8 5 2 6 4 5 8 7 5 4 9 6 3 7 5 6 9 8 6 5 1 7 4 8 6 7 1 9 7 6 2 8 5)
    #(2 3 1 4 2 4 9 6 3 2 3 4 2 5 3 5 1 7 4 3 4 5 3 6 4 6 2 8 5 4 5 6 4 7 5 7 3 9 6 5 6 7 5 8 6 8 4 1 7 6)
    #(3 4 2 2 1 5 5 6 9 2 4 5 3 3 2 6 6 7 1 3 5 6 4 4 3 7 7 8 2 4 6 7 5 5 4 8 8 9 3 5 7 8 6 6 5 9 9 1 4 6)
    #(3 3 8 5 9 7 3 9 6 4 4 4 9 6 1 8 4 1 7 5 5 5 1 7 2 9 5 2 8 6 6 6 2 8 3 1 6 3 9 7 7 7 3 9 4 2 7 4 1 8)
    #(3 5 1 3 5 9 5 8 9 4 4 6 2 4 6 1 6 9 1 5 5 7 3 5 7 2 7 1 2 6 6 8 4 6 8 3 8 2 3 7 7 9 5 7 9 4 9 3 4 8)
    #(4 3 5 8 7 3 3 5 4 1 5 4 6 9 8 4 4 6 5 2 6 5 7 1 9 5 5 7 6 3 7 6 8 2 1 6 6 8 7 4 8 7 9 3 2 7 7 9 8 5)
    #(5 8 2 6 2 5 3 7 8 2 6 9 3 7 3 6 4 8 9 3 7 1 4 8 4 7 5 9 1 4 8 2 5 9 5 8 6 1 2 5 9 3 6 1 6 9 7 2 3 6)
    #(9 6 8 5 6 3 9 3 3 3 1 7 9 6 7 4 1 4 4 4 2 8 1 7 8 5 2 5 5 5 3 9 2 8 9 6 3 6 6 6 4 1 3 9 1 7 4 7 7 7)
    #(3 5 3 2 3 4 1 3 5 9 4 6 4 3 4 5 2 4 6 1 5 7 5 4 5 6 3 5 7 2 6 8 6 5 6 7 4 6 8 3 7 9 7 6 7 8 5 7 9 4)
    #(3 5 7 2 2 3 4 6 4 3 4 6 8 3 3 4 5 7 5 4 5 7 9 4 4 5 6 8 6 5 6 8 1 5 5 6 7 9 7 6 7 9 2 6 6 7 8 1 8 7)
    #(5 3 4 7 6 4 3 8 5 2 6 4 5 8 7 5 4 9 6 3 7 5 6 9 8 6 5 1 7 4 8 6 7 1 9 7 6 2 8 5 9 7 8 2 1 8 7 3 9 6)
    #(3 4 2 5 3 5 1 7 4 3 4 5 3 6 4 6 2 8 5 4 5 6 4 7 5 7 3 9 6 5 6 7 5 8 6 8 4 1 7 6 7 8 6 9 7 9 5 2 8 7)
    #(4 5 3 3 2 6 6 7 1 3 5 6 4 4 3 7 7 8 2 4 6 7 5 5 4 8 8 9 3 5 7 8 6 6 5 9 9 1 4 6 8 9 7 7 6 1 1 2 5 7)
    #(4 4 9 6 1 8 4 1 7 5 5 5 1 7 2 9 5 2 8 6 6 6 2 8 3 1 6 3 9 7 7 7 3 9 4 2 7 4 1 8 8 8 4 1 5 3 8 5 2 9)
    #(4 6 2 4 6 1 6 9 1 5 5 7 3 5 7 2 7 1 2 6 6 8 4 6 8 3 8 2 3 7 7 9 5 7 9 4 9 3 4 8 8 1 6 8 1 5 1 4 5 9)
    #(5 4 6 9 8 4 4 6 5 2 6 5 7 1 9 5 5 7 6 3 7 6 8 2 1 6 6 8 7 4 8 7 9 3 2 7 7 9 8 5 9 8 1 4 3 8 8 1 9 6)
    #(6 9 3 7 3 6 4 8 9 3 7 1 4 8 4 7 5 9 1 4 8 2 5 9 5 8 6 1 2 5 9 3 6 1 6 9 7 2 3 6 1 4 7 2 7 1 8 3 4 7)
    #(1 7 9 6 7 4 1 4 4 4 2 8 1 7 8 5 2 5 5 5 3 9 2 8 9 6 3 6 6 6 4 1 3 9 1 7 4 7 7 7 5 2 4 1 2 8 5 8 8 8)
    #(4 6 4 3 4 5 2 4 6 1 5 7 5 4 5 6 3 5 7 2 6 8 6 5 6 7 4 6 8 3 7 9 7 6 7 8 5 7 9 4 8 1 8 7 8 9 6 8 1 5)
    #(4 6 8 3 3 4 5 7 5 4 5 7 9 4 4 5 6 8 6 5 6 8 1 5 5 6 7 9 7 6 7 9 2 6 6 7 8 1 8 7 8 1 3 7 7 8 9 2 9 8)
    #(6 4 5 8 7 5 4 9 6 3 7 5 6 9 8 6 5 1 7 4 8 6 7 1 9 7 6 2 8 5 9 7 8 2 1 8 7 3 9 6 1 8 9 3 2 9 8 4 1 7)
    #(4 5 3 6 4 6 2 8 5 4 5 6 4 7 5 7 3 9 6 5 6 7 5 8 6 8 4 1 7 6 7 8 6 9 7 9 5 2 8 7 8 9 7 1 8 1 6 3 9 8)
    #(5 6 4 4 3 7 7 8 2 4 6 7 5 5 4 8 8 9 3 5 7 8 6 6 5 9 9 1 4 6 8 9 7 7 6 1 1 2 5 7 9 1 8 8 7 2 2 3 6 8)
    #(5 5 1 7 2 9 5 2 8 6 6 6 2 8 3 1 6 3 9 7 7 7 3 9 4 2 7 4 1 8 8 8 4 1 5 3 8 5 2 9 9 9 5 2 6 4 9 6 3 1)
    #(5 7 3 5 7 2 7 1 2 6 6 8 4 6 8 3 8 2 3 7 7 9 5 7 9 4 9 3 4 8 8 1 6 8 1 5 1 4 5 9 9 2 7 9 2 6 2 5 6 1)
    #(6 5 7 1 9 5 5 7 6 3 7 6 8 2 1 6 6 8 7 4 8 7 9 3 2 7 7 9 8 5 9 8 1 4 3 8 8 1 9 6 1 9 2 5 4 9 9 2 1 7)
    #(7 1 4 8 4 7 5 9 1 4 8 2 5 9 5 8 6 1 2 5 9 3 6 1 6 9 7 2 3 6 1 4 7 2 7 1 8 3 4 7 2 5 8 3 8 2 9 4 5 8)
    #(2 8 1 7 8 5 2 5 5 5 3 9 2 8 9 6 3 6 6 6 4 1 3 9 1 7 4 7 7 7 5 2 4 1 2 8 5 8 8 8 6 3 5 2 3 9 6 9 9 9)
    #(5 7 5 4 5 6 3 5 7 2 6 8 6 5 6 7 4 6 8 3 7 9 7 6 7 8 5 7 9 4 8 1 8 7 8 9 6 8 1 5 9 2 9 8 9 1 7 9 2 6)
    #(5 7 9 4 4 5 6 8 6 5 6 8 1 5 5 6 7 9 7 6 7 9 2 6 6 7 8 1 8 7 8 1 3 7 7 8 9 2 9 8 9 2 4 8 8 9 1 3 1 9)
    #(7 5 6 9 8 6 5 1 7 4 8 6 7 1 9 7 6 2 8 5 9 7 8 2 1 8 7 3 9 6 1 8 9 3 2 9 8 4 1 7 2 9 1 4 3 1 9 5 2 8)
    #(5 6 4 7 5 7 3 9 6 5 6 7 5 8 6 8 4 1 7 6 7 8 6 9 7 9 5 2 8 7 8 9 7 1 8 1 6 3 9 8 9 1 8 2 9 2 7 4 1 9)
    #(6 7 5 5 4 8 8 9 3 5 7 8 6 6 5 9 9 1 4 6 8 9 7 7 6 1 1 2 5 7 9 1 8 8 7 2 2 3 6 8 1 2 9 9 8 3 3 4 7 9)
    ))





(define input
  #(
    #(4 5 5 2 2 8 5 9 8 9 4 4 1 1 2 4 7 1 9 7 9 8 4 6 5 8 2 5 7 7 3 3 1 8 2 5 2 2 6 9 4 2 2 6 2 5 7 3 1 6 2 8 8 5 1 1 5 5 1 2 3 1 2 3 6 8 7 1 5 1 4 1 2 1 7 1 2 2 4 2 9 8 2 7 1 6 2 5 2 4 9 6 6 2 3 2 8 1 7 5)
    #(8 3 1 1 8 2 6 9 1 7 6 7 7 2 9 5 5 6 1 3 9 5 7 1 6 2 4 5 4 2 6 1 8 4 2 1 2 4 7 1 3 1 1 2 1 4 8 5 1 3 5 1 5 4 7 8 4 2 1 2 4 1 6 5 6 7 4 9 2 2 6 4 4 7 1 9 9 3 4 1 1 3 2 1 7 1 3 4 5 3 1 1 1 1 7 1 1 8 4 3)
    #(3 1 3 4 1 5 5 5 2 8 2 3 1 2 9 6 3 1 3 2 9 2 9 5 1 6 8 9 3 7 1 6 7 1 6 1 1 3 5 5 1 9 9 4 2 9 3 1 3 2 6 1 2 2 6 3 9 2 9 9 1 1 2 9 9 5 9 2 9 3 2 1 2 4 6 1 2 8 9 4 1 1 3 9 3 2 6 3 1 8 1 2 1 3 1 2 2 3 9 8)
    #(7 1 3 4 1 5 9 6 8 1 5 3 3 3 3 1 1 7 2 9 9 1 9 3 2 8 1 9 1 2 8 7 1 8 1 3 1 1 2 1 4 2 8 7 1 5 1 9 1 2 3 8 6 1 2 6 2 3 8 9 1 2 3 3 1 1 2 5 3 4 2 3 1 4 8 2 6 8 1 1 8 2 3 5 4 1 2 9 7 9 1 9 1 1 3 3 9 1 3 1)
    #(3 2 9 1 6 2 2 1 2 1 8 9 3 8 2 1 2 5 8 2 1 5 1 1 1 5 3 1 6 1 4 2 3 3 9 9 9 6 6 1 6 4 9 2 7 1 8 1 1 8 7 1 5 2 9 4 6 1 2 1 1 1 1 5 7 3 9 8 2 1 3 8 8 9 7 3 6 1 1 1 8 3 5 1 7 6 5 1 5 6 7 3 1 4 8 4 6 3 4 9)
    #(9 1 1 6 1 9 7 3 1 9 9 9 2 2 2 3 7 8 4 9 7 3 7 4 9 1 5 9 9 1 2 9 1 8 9 4 8 5 1 4 8 5 5 2 1 8 9 5 7 3 2 9 9 7 1 4 1 6 1 7 7 1 5 2 1 7 4 1 6 9 4 9 7 8 1 2 3 7 3 2 1 9 9 8 4 1 3 3 1 2 1 1 5 5 3 1 9 1 1 3)
    #(4 9 6 8 4 3 9 1 1 4 1 3 5 9 9 6 9 2 3 9 3 9 6 2 1 5 2 1 2 2 7 4 3 8 6 1 6 4 3 2 2 6 2 9 6 2 5 7 2 5 5 3 4 5 1 3 3 3 1 4 1 5 9 1 6 4 1 1 2 1 3 1 1 1 2 9 9 1 3 8 1 2 8 9 2 2 9 3 1 7 9 1 1 4 2 3 1 7 2 1)
    #(6 2 3 9 2 1 9 7 8 5 2 3 4 6 7 5 3 9 1 2 8 1 6 4 1 1 1 4 1 4 8 2 2 5 9 4 2 6 2 2 1 9 2 5 4 2 1 1 3 7 1 2 3 2 7 3 6 1 2 2 3 7 1 6 9 5 1 1 9 1 2 7 5 7 3 6 3 8 8 8 4 5 6 2 3 9 9 4 8 9 1 7 9 9 5 9 2 9 2 8)
    #(1 9 3 9 2 7 3 1 5 2 1 1 4 1 6 5 8 1 5 6 2 1 8 9 8 1 3 1 7 4 1 6 2 5 1 4 2 2 7 3 7 5 7 4 1 1 1 4 1 6 7 3 1 1 2 2 5 9 8 9 7 3 5 8 2 9 9 2 2 2 2 6 9 1 9 1 1 9 1 2 1 4 3 2 1 7 5 8 2 4 5 3 2 6 1 9 9 8 1 4)
    #(3 8 1 6 2 9 5 1 2 1 2 1 1 7 1 3 9 1 9 9 1 8 1 2 9 1 4 1 5 2 1 7 4 8 3 1 5 5 7 9 4 1 3 8 5 4 2 7 3 1 2 5 3 1 1 8 2 3 4 3 4 6 7 1 3 6 5 1 3 3 6 3 6 9 8 1 1 1 2 3 6 7 4 5 1 2 9 4 9 7 2 2 3 8 7 9 4 4 4 3)
    #(2 8 5 2 1 7 4 3 9 2 1 7 5 2 1 7 8 9 1 2 9 2 7 6 9 2 9 7 3 2 9 7 3 4 1 4 1 5 2 9 2 8 8 3 1 8 1 8 1 2 2 1 9 5 3 2 1 2 1 3 7 3 1 3 8 5 8 9 1 1 2 2 2 7 2 7 4 2 4 1 3 4 3 1 9 7 8 2 3 2 4 8 3 9 6 1 6 2 2 9)
    #(8 1 8 3 4 1 1 9 8 1 1 3 6 7 1 8 4 2 8 8 2 3 1 2 7 6 8 1 4 1 1 2 4 6 5 2 8 2 2 2 1 3 8 4 2 1 8 7 1 7 7 8 2 5 1 2 3 1 4 2 3 1 7 1 5 8 8 6 4 2 3 2 8 3 9 5 1 1 4 1 2 3 8 1 1 1 2 5 6 1 2 1 1 8 6 2 6 2 1 8)
    #(3 3 3 8 4 1 8 9 5 8 8 1 8 2 2 9 3 3 1 3 9 9 5 1 3 6 9 1 9 2 1 9 2 1 8 6 9 9 2 2 4 1 3 1 3 6 1 9 4 4 2 1 8 2 8 2 9 1 1 9 7 1 3 2 7 9 1 8 1 2 2 2 2 2 4 1 6 3 3 4 6 1 4 2 6 9 3 8 6 2 1 4 6 2 1 1 8 9 9 9)
    #(6 1 1 8 5 8 2 7 6 5 3 7 5 5 4 1 1 4 4 4 9 1 5 1 9 8 7 1 1 2 3 1 2 1 1 7 2 9 4 3 6 7 8 8 7 3 2 4 7 5 4 1 9 2 1 1 9 2 9 1 5 1 5 1 7 1 7 9 5 7 8 1 2 7 1 4 8 4 1 1 4 5 2 2 3 1 6 1 1 1 6 2 4 4 1 4 1 7 1 9)
    #(1 4 6 9 2 5 7 4 1 8 3 1 3 2 1 8 9 5 1 2 1 1 5 1 1 5 9 1 2 2 4 2 8 1 8 6 8 4 3 3 2 8 5 5 1 8 8 9 6 5 7 1 8 6 8 9 7 3 9 9 6 7 7 6 3 9 1 9 1 6 4 2 1 1 7 9 8 6 3 5 2 4 3 4 2 1 4 4 1 6 6 2 7 3 7 8 3 9 1 2)
    #(1 4 2 2 1 1 8 4 1 9 3 1 9 5 1 5 4 9 6 1 8 9 3 9 1 1 5 8 5 1 6 4 6 2 2 7 7 3 9 3 6 2 2 1 2 9 9 3 6 8 7 1 9 8 2 2 1 7 5 6 2 4 5 7 1 2 9 9 6 4 2 1 9 9 1 2 1 2 4 1 2 3 2 1 8 3 1 2 9 7 7 3 1 3 6 8 2 8 1 1)
    #(8 1 6 2 5 4 6 1 3 9 9 5 2 1 3 6 1 8 9 9 1 1 1 7 7 5 3 3 3 5 7 8 2 1 2 5 2 8 9 1 9 7 5 9 6 5 2 1 8 1 5 2 1 8 4 1 1 2 1 3 2 3 6 7 2 4 5 4 8 6 7 4 7 2 1 2 2 1 4 3 3 3 3 2 4 2 2 1 1 1 9 9 5 1 5 2 8 1 1 4)
    #(1 4 8 8 2 3 9 6 1 1 4 6 1 4 9 1 9 7 3 8 6 5 1 8 5 2 4 2 9 5 9 5 7 7 7 5 2 9 1 4 1 9 1 8 1 7 4 7 7 5 2 3 1 7 9 1 4 3 6 7 1 1 4 8 3 1 6 5 4 2 5 1 7 2 1 3 2 8 1 1 1 1 1 5 1 1 1 9 1 1 1 8 6 4 9 5 9 3 9 2)
    #(2 2 1 8 9 1 7 6 6 1 8 6 8 1 3 3 1 4 1 2 4 3 8 1 9 9 6 2 4 1 3 2 1 6 1 3 1 9 7 9 9 1 3 2 3 2 7 9 9 9 8 8 8 7 1 9 1 3 9 3 9 1 1 1 8 2 3 3 1 7 4 2 1 7 1 2 8 2 4 9 9 4 5 4 2 9 9 2 7 9 2 2 5 5 2 2 3 5 6 1)
    #(5 7 3 4 3 2 6 7 3 2 8 8 4 7 9 2 4 2 8 1 9 1 2 4 1 1 2 1 1 9 1 8 1 1 1 1 1 1 1 1 4 7 3 3 8 7 9 5 9 6 5 3 9 3 8 2 6 9 2 1 4 1 1 2 1 9 2 2 9 6 1 6 2 1 1 1 1 2 1 2 7 1 8 8 9 6 2 1 2 1 1 1 1 8 1 9 5 1 1 1)
    #(1 5 1 1 9 3 5 1 1 1 8 6 1 1 2 1 9 3 5 7 4 1 7 6 8 3 3 8 9 2 3 1 2 8 1 2 4 6 6 1 8 1 9 1 9 1 9 2 5 1 3 7 1 2 1 5 9 1 3 2 1 2 2 9 1 2 1 1 1 1 7 5 5 2 1 9 8 1 1 2 1 2 1 8 2 1 2 1 7 4 2 7 7 8 1 7 2 2 2 4)
    #(9 6 9 1 5 6 2 1 8 3 5 1 3 3 8 8 2 2 1 7 2 6 4 9 3 8 2 1 2 3 8 4 1 9 1 2 1 4 1 1 1 4 1 6 5 2 4 1 5 7 2 4 2 5 7 1 6 9 1 7 5 5 1 1 9 8 4 4 1 9 8 2 6 3 1 7 9 1 2 1 1 9 9 9 9 4 3 4 6 7 8 1 2 1 3 3 9 2 3 1)
    #(2 7 2 9 2 3 7 5 3 2 9 5 9 4 4 4 7 3 3 2 2 5 4 3 5 9 9 1 1 1 9 4 6 1 9 2 1 9 1 6 8 8 2 4 3 5 5 8 6 1 1 2 3 1 1 2 2 3 1 2 4 1 4 1 1 8 6 3 1 2 4 8 8 2 1 8 1 2 9 1 4 9 8 1 2 5 1 5 6 5 8 1 7 1 8 6 3 4 2 5)
    #(7 3 1 2 5 5 9 1 8 1 3 2 3 7 4 3 9 3 8 2 2 1 2 1 2 2 3 6 1 9 1 5 1 4 3 1 8 4 1 8 5 4 8 1 8 6 1 3 9 3 1 7 2 3 3 7 3 6 1 7 1 3 3 3 4 2 1 4 1 6 2 1 2 2 3 7 1 9 2 1 1 9 3 5 5 1 9 2 4 7 1 9 8 9 8 9 2 2 1 2)
    #(9 4 1 5 7 4 7 7 3 8 3 4 6 1 4 1 6 9 1 4 1 8 8 2 5 1 9 7 1 7 3 1 9 2 5 1 5 2 1 9 2 6 6 1 7 2 1 3 6 4 5 5 4 5 2 8 1 5 1 6 2 5 1 7 4 7 5 8 8 3 8 6 3 1 4 3 2 3 9 1 1 2 1 1 6 1 4 1 8 4 1 1 4 8 4 5 1 7 4 9)
    #(1 7 1 6 2 6 3 1 2 1 1 1 5 6 3 1 1 4 1 7 8 4 2 5 2 2 5 1 3 1 4 5 8 8 4 1 3 2 1 3 8 1 4 5 1 1 6 9 1 4 5 1 3 9 3 9 1 1 9 2 4 8 1 1 5 7 2 8 6 1 1 4 4 2 3 5 9 9 1 2 9 2 1 7 5 2 5 5 3 7 2 1 3 3 8 1 1 3 1 5)
    #(2 1 1 3 4 1 4 9 1 1 7 3 1 1 6 4 9 6 2 3 4 2 2 2 3 4 2 1 3 1 7 9 2 1 6 4 7 4 6 1 4 9 1 2 6 8 1 1 7 3 8 6 8 1 5 4 3 3 9 1 1 1 1 2 5 4 1 5 1 1 1 3 1 1 1 4 7 6 1 9 5 9 2 5 3 1 2 3 1 2 8 1 1 2 8 1 9 3 1 2)
    #(5 6 1 1 9 1 2 3 3 4 9 2 7 2 2 1 2 5 2 7 1 1 2 7 6 8 7 1 2 1 1 6 2 4 9 1 1 5 6 4 1 6 2 1 6 1 1 5 1 2 9 1 1 4 1 4 7 1 4 9 4 5 5 5 2 2 1 6 2 4 1 1 2 2 4 6 6 1 4 1 5 1 5 9 1 1 1 1 9 2 3 2 6 5 7 7 6 2 9 1)
    #(1 2 3 2 3 1 3 2 7 1 1 1 3 5 5 4 1 8 9 9 5 5 4 1 3 1 5 5 1 1 2 6 9 9 1 6 3 1 1 4 8 2 1 9 1 1 3 5 3 1 7 1 3 4 1 2 1 5 4 8 5 2 5 2 6 4 7 8 8 9 6 4 6 6 1 9 2 3 6 2 5 3 6 8 8 4 8 2 3 5 1 3 1 5 7 3 5 3 6 9)
    #(2 9 2 8 1 2 3 1 2 1 1 9 1 2 6 7 7 2 1 3 2 3 8 2 2 1 3 3 3 8 4 3 2 8 4 1 2 6 1 6 1 1 9 2 4 6 1 7 8 1 3 1 3 2 1 2 4 5 6 9 3 1 9 2 6 1 7 1 5 1 3 4 8 4 8 9 2 7 1 9 9 3 4 6 8 5 7 4 2 1 3 2 3 4 9 2 3 1 6 1)
    #(3 7 3 8 1 5 2 5 4 4 6 2 5 5 1 6 9 1 6 6 9 3 6 4 2 7 3 8 5 7 3 1 9 3 8 1 8 3 7 9 6 3 5 2 4 1 1 1 2 2 9 9 5 6 8 6 1 1 5 1 1 4 3 2 1 7 4 5 1 9 8 5 5 1 3 3 8 5 9 2 2 7 1 3 9 7 1 2 6 9 1 9 3 1 1 6 3 4 5 4)
    #(4 9 7 8 9 3 2 2 3 7 3 1 2 6 1 5 4 5 1 1 1 5 1 2 3 9 9 4 1 9 4 1 8 1 3 8 7 2 3 9 4 6 1 9 8 5 2 1 8 4 5 4 5 9 6 6 6 4 7 1 4 6 9 4 1 6 9 8 1 2 5 9 1 2 5 1 5 4 2 1 2 8 7 9 1 1 1 2 5 8 8 2 3 8 4 2 8 1 1 3)
    #(3 9 1 2 8 9 9 1 8 2 7 9 8 9 1 6 6 2 8 9 1 8 1 3 1 9 7 9 3 7 1 1 1 2 2 2 6 9 5 3 5 1 2 8 1 4 1 1 1 3 1 1 1 2 8 3 8 1 1 1 9 5 4 8 2 2 1 1 9 9 6 1 6 4 7 8 8 6 8 3 3 1 8 7 5 3 3 3 1 8 6 4 8 1 1 2 1 1 7 5)
    #(1 1 9 2 9 2 7 4 2 1 3 7 2 5 8 4 2 3 2 1 4 1 9 1 5 1 1 1 8 8 6 4 9 9 5 2 9 4 9 2 2 7 8 6 3 6 7 6 9 1 4 5 7 9 3 9 8 5 1 3 9 2 5 9 8 1 2 3 7 4 3 6 9 3 3 1 9 8 2 9 3 1 4 7 1 4 2 1 3 3 6 1 3 1 1 5 1 6 2 5)
    #(3 9 1 1 1 8 5 2 1 5 5 8 2 4 3 4 8 4 4 1 9 3 1 6 1 9 2 5 1 5 1 1 2 1 2 8 6 7 2 1 2 9 2 8 4 2 8 1 1 1 1 9 3 6 2 2 1 1 4 8 4 1 1 2 1 4 3 9 2 1 9 5 6 2 1 1 7 2 4 1 5 7 1 2 2 5 3 9 8 3 7 4 9 2 3 9 4 4 9 2)
    #(2 9 5 3 2 1 6 7 2 2 8 1 5 2 1 2 6 3 2 7 3 2 4 5 3 1 1 7 1 2 7 3 2 4 3 3 4 2 4 9 6 8 2 2 3 3 3 7 1 2 1 1 6 1 4 7 6 2 7 2 2 3 1 3 1 8 6 9 4 7 2 9 5 7 9 5 9 8 3 3 9 2 3 1 1 1 2 1 8 2 6 1 9 7 1 6 1 6 2 4)
    #(1 1 9 8 3 8 2 7 6 7 2 1 8 3 1 2 2 7 9 6 8 2 2 7 8 1 9 1 8 2 2 3 7 4 5 5 9 2 9 3 7 9 3 8 7 3 1 7 4 4 9 9 2 1 1 5 1 6 4 3 6 3 7 2 4 3 1 3 7 1 9 6 1 6 4 8 2 8 2 1 4 6 2 8 3 9 3 5 2 9 3 2 2 4 8 8 3 9 1 2)
    #(2 7 2 1 8 8 4 4 1 2 9 4 9 1 4 7 6 1 3 3 4 2 9 4 1 1 8 4 1 9 2 8 1 1 7 9 9 6 6 9 7 7 1 9 1 5 1 2 2 1 4 9 9 7 4 2 9 4 6 3 8 9 8 1 6 4 1 3 7 7 3 2 4 2 3 8 7 3 1 5 3 9 9 1 4 3 6 9 6 8 1 3 4 2 3 9 4 4 7 7)
    #(3 1 1 5 1 1 8 2 2 6 2 5 7 1 1 2 1 7 2 2 1 4 5 3 1 4 8 4 1 9 1 9 7 9 1 2 1 2 1 2 6 2 9 6 1 1 9 4 1 8 2 9 7 2 9 4 7 5 1 4 2 2 1 4 9 1 9 2 1 6 1 8 3 1 1 2 7 9 8 1 1 9 1 3 2 1 3 1 2 9 9 9 2 9 9 2 9 6 9 9)
    #(1 4 6 6 1 3 8 1 5 2 5 8 9 9 1 2 2 5 3 1 2 9 9 8 1 8 1 6 4 1 4 3 3 1 4 5 1 7 1 8 3 2 1 3 4 5 7 9 5 9 8 1 1 1 1 8 9 1 2 8 1 7 8 1 1 1 1 9 7 2 1 8 4 9 1 9 8 2 1 9 2 5 9 2 3 1 4 1 3 4 4 1 2 7 1 1 9 9 1 6)
    #(9 1 2 2 1 3 4 4 5 2 8 4 6 2 2 6 1 1 1 1 5 8 1 2 8 4 9 1 2 9 1 1 2 1 1 4 5 9 2 1 2 2 2 6 1 1 1 9 1 9 4 8 9 3 1 8 1 1 1 2 3 7 5 1 2 2 6 2 2 1 7 1 6 7 1 9 1 6 6 8 4 3 7 6 6 8 2 3 6 1 5 4 6 3 2 7 1 9 3 2)
    #(6 6 1 9 4 1 3 2 3 1 5 9 5 2 4 1 5 1 8 5 1 9 7 9 1 3 4 1 3 5 9 8 8 6 5 6 2 9 8 1 1 1 6 1 1 1 7 1 2 9 1 7 2 3 3 7 3 1 1 6 2 1 9 1 9 9 1 2 3 9 4 3 2 6 5 2 3 8 6 9 3 1 1 5 9 5 9 1 3 2 6 1 8 7 1 1 3 8 1 7)
    #(1 5 9 1 8 4 5 5 5 9 1 6 2 7 7 1 8 4 7 3 2 1 1 5 2 8 2 5 1 8 7 2 2 1 3 9 1 2 1 1 1 2 2 5 4 1 2 8 1 2 4 4 7 5 4 1 1 9 2 9 2 9 7 9 5 4 8 9 8 4 4 9 3 1 6 2 9 9 1 9 1 9 9 5 9 7 1 4 9 3 4 3 2 2 3 3 7 9 1 6)
    #(9 1 4 5 6 7 2 2 9 4 2 1 2 1 4 2 4 3 9 2 1 3 8 1 1 9 3 1 9 7 6 3 7 2 2 4 7 3 6 2 1 1 4 8 2 9 3 3 9 2 1 1 1 3 1 1 2 8 2 8 1 8 6 5 1 7 4 1 1 3 9 1 2 1 2 1 4 9 8 4 9 2 3 2 4 1 3 1 1 4 8 1 2 1 4 1 2 3 4 1)
    #(1 7 5 6 3 8 1 9 6 7 9 1 3 4 3 2 2 2 2 1 1 2 2 2 6 1 9 3 1 3 2 1 1 1 1 2 1 1 6 3 7 3 1 1 9 2 1 3 1 1 4 3 7 8 2 1 1 9 2 9 9 3 1 1 6 6 6 7 5 1 4 2 2 5 1 1 1 9 3 1 6 1 9 1 2 3 1 1 1 7 7 2 5 5 3 1 1 4 9 1)
    #(7 1 1 5 4 2 1 9 1 6 7 5 2 2 6 6 1 9 7 8 1 1 9 1 8 1 4 6 2 3 7 9 5 3 7 1 2 8 1 7 1 3 1 3 9 1 6 3 3 9 1 3 8 2 1 8 1 4 5 8 2 9 3 1 4 1 2 5 1 3 3 8 1 9 3 2 1 4 2 1 3 8 3 7 1 1 2 5 2 4 6 1 8 9 9 1 4 1 2 6)
    #(1 5 9 8 7 5 9 3 7 6 3 9 2 5 1 4 4 1 7 8 2 2 1 9 5 2 8 4 1 1 8 1 3 3 4 7 2 2 1 1 8 7 2 1 8 3 4 2 4 1 3 1 3 6 1 2 9 2 7 8 1 1 1 3 4 4 2 7 5 1 7 4 5 4 3 2 1 7 3 3 3 7 6 1 2 9 7 3 6 4 2 1 8 1 8 1 9 9 1 1)
    #(1 5 9 1 9 2 7 5 1 4 9 8 6 4 3 1 5 4 2 9 3 1 3 3 1 1 3 9 6 1 3 9 2 4 2 1 2 5 1 5 8 3 1 9 2 1 3 5 9 2 6 1 7 5 5 4 2 4 1 1 7 2 3 2 6 1 9 1 1 6 2 5 7 3 5 8 7 3 2 8 1 1 3 6 1 5 5 1 4 3 3 1 3 3 7 1 5 4 2 9)
    #(2 2 1 2 1 1 1 7 8 6 2 6 8 5 1 8 3 8 8 6 7 7 9 2 3 2 1 1 5 3 3 1 8 8 3 4 5 9 3 9 1 4 1 4 3 1 4 2 3 3 2 2 7 4 1 1 8 5 3 8 8 8 1 8 7 4 6 9 3 7 9 3 3 3 1 6 1 3 9 5 9 6 9 6 3 9 1 7 1 1 5 1 5 6 5 1 3 6 2 8)
    #(1 8 5 5 5 4 6 8 6 9 7 8 4 8 5 7 7 3 6 2 1 4 5 5 1 4 5 7 2 6 2 7 6 9 2 1 5 5 2 9 8 3 2 1 2 1 2 6 1 2 3 1 6 1 3 4 3 3 3 9 9 9 6 2 4 9 6 1 5 3 4 1 4 7 9 2 1 1 9 7 1 4 4 1 9 1 5 3 3 2 8 4 4 7 3 1 9 9 2 1)
    #(3 5 1 1 4 1 5 8 7 1 7 1 3 3 4 1 5 1 7 2 5 9 5 6 5 2 3 2 1 1 6 4 3 3 9 8 1 3 5 1 4 2 1 6 7 8 1 3 5 3 2 1 1 9 1 4 1 5 2 2 1 8 3 7 2 9 5 4 1 4 2 9 2 7 6 4 7 8 8 2 3 1 1 1 4 8 2 6 3 1 2 1 6 2 3 4 1 8 3 4)
    #(1 3 1 8 1 2 6 2 1 9 1 1 9 4 2 3 3 5 8 2 2 3 3 9 3 7 2 2 1 1 4 2 3 2 9 1 9 9 1 1 5 1 3 1 1 9 4 3 8 1 4 8 8 7 8 4 1 3 5 3 8 9 1 1 4 8 8 3 2 3 9 9 2 2 5 4 2 5 2 9 1 4 8 5 7 2 7 2 1 3 9 9 2 9 3 1 5 5 6 3)
    #(4 2 8 9 2 2 6 8 7 2 4 1 9 2 9 1 8 6 1 1 1 1 3 8 3 5 3 8 1 1 1 2 1 9 1 1 2 7 2 3 1 2 1 1 3 1 5 9 1 9 7 1 6 8 8 2 9 3 1 1 6 1 9 1 2 4 3 1 7 2 3 2 7 9 1 2 1 2 2 5 3 1 1 5 1 2 2 8 9 3 1 1 6 3 8 2 5 3 6 1)
    #(4 2 6 7 1 5 8 1 3 9 1 9 8 9 2 3 5 7 7 6 1 5 6 2 8 1 5 7 7 6 7 2 1 5 3 3 1 5 2 1 9 8 9 2 2 7 2 2 9 2 7 3 4 4 3 3 2 9 7 2 1 6 9 8 9 2 9 3 7 3 2 6 9 1 2 1 1 2 2 1 1 1 2 1 1 1 1 8 2 3 4 8 8 5 2 1 6 4 1 3)
    #(1 7 2 1 2 8 1 3 9 2 3 5 3 2 4 8 9 1 8 1 7 1 1 1 1 3 4 8 4 3 1 6 3 2 1 9 2 8 8 5 9 8 3 4 3 3 1 9 2 9 1 1 6 7 3 1 4 3 2 1 2 3 1 4 8 3 5 8 2 4 2 5 1 4 3 7 3 9 2 1 6 3 1 3 1 1 7 2 6 7 1 1 3 3 2 1 4 2 1 1)
    #(5 6 9 2 4 3 4 7 6 7 1 6 7 2 1 6 2 4 6 1 8 1 9 1 1 6 4 7 4 8 7 6 2 8 1 5 1 1 9 9 6 3 1 6 2 9 9 1 9 2 5 6 7 4 2 2 6 1 1 1 3 1 6 6 1 1 4 1 4 9 9 1 1 9 8 6 9 1 1 2 1 7 6 4 4 9 1 4 3 2 9 3 9 1 5 1 1 7 1 2)
    #(3 1 7 1 7 7 8 9 7 9 1 3 3 3 8 4 8 2 1 4 1 5 9 1 1 9 1 5 9 3 1 1 1 2 6 1 4 4 1 1 1 6 1 4 4 3 5 1 9 1 8 8 2 2 1 5 1 8 1 1 7 9 9 1 1 5 1 8 2 2 1 9 3 1 2 1 4 7 1 2 1 2 1 1 8 5 1 6 1 7 1 6 1 2 1 9 4 4 8 1)
    #(5 7 2 8 8 9 2 9 1 5 3 9 4 1 1 6 5 3 5 6 8 5 1 3 2 8 4 1 5 2 1 3 1 4 9 1 4 1 2 1 1 5 5 6 3 6 5 1 2 1 2 9 9 4 9 6 5 9 4 3 1 2 9 1 1 3 4 1 3 9 3 3 8 4 1 3 1 2 7 3 2 1 1 1 5 7 9 1 6 1 7 1 5 8 2 1 9 4 9 2)
    #(5 9 2 2 8 6 1 3 1 9 1 6 1 2 1 1 5 9 5 2 8 7 2 3 2 1 1 2 2 1 9 4 3 7 8 1 1 2 2 2 3 5 2 6 6 6 1 5 1 9 2 9 9 9 5 4 3 1 1 1 1 4 4 4 3 6 4 1 1 9 3 2 1 3 5 8 2 5 6 3 1 4 9 8 1 4 4 7 2 8 3 2 6 1 2 3 9 1 8 4)
    #(3 2 2 2 9 2 2 8 8 1 2 1 3 8 1 3 5 3 8 1 1 2 2 2 1 2 1 9 9 1 9 8 3 9 2 2 2 5 6 2 1 3 8 2 2 2 2 1 4 3 7 3 5 8 8 4 1 5 7 2 1 9 9 7 3 4 5 2 7 3 1 1 5 2 2 1 9 2 6 2 2 1 1 7 1 7 8 8 3 2 3 4 2 5 3 9 8 2 2 1)
    #(7 1 2 1 8 9 2 2 8 9 4 7 1 7 5 7 7 1 4 1 6 4 1 9 2 3 1 7 1 9 9 8 8 9 3 2 1 5 2 5 2 8 9 1 6 1 1 9 1 6 8 4 6 2 7 1 2 7 2 3 1 6 2 2 1 2 1 8 1 3 2 2 8 1 6 4 9 2 8 1 7 1 7 1 1 8 8 3 1 3 1 4 1 1 1 7 1 1 1 5)
    #(4 1 4 2 1 3 3 1 9 5 6 3 1 8 9 1 9 1 1 1 4 9 4 3 9 2 1 3 6 6 2 2 1 1 4 3 3 8 3 7 3 2 1 2 9 6 3 8 1 4 6 5 8 1 6 1 7 3 9 9 7 7 5 9 4 9 2 2 2 5 9 4 2 2 8 4 8 2 1 4 9 2 3 1 7 3 9 2 2 2 6 3 1 6 1 7 1 4 2 1)
    #(7 1 7 1 7 6 7 9 5 2 2 1 8 5 1 6 8 5 9 1 1 1 9 2 1 3 8 2 9 8 1 4 1 2 1 1 7 8 4 5 1 5 5 5 3 1 4 1 4 9 2 9 2 8 5 8 4 4 6 1 9 2 8 1 3 3 8 1 5 1 9 1 3 7 4 6 9 2 2 2 2 9 4 1 8 5 2 1 3 2 9 5 9 5 3 2 9 1 3 2)
    #(4 8 1 6 4 3 4 4 8 9 2 2 5 6 1 5 1 2 9 1 1 2 9 7 8 1 1 1 5 1 1 3 1 9 2 2 2 1 6 3 3 5 7 8 7 2 5 1 1 8 9 1 3 2 1 8 7 2 1 3 6 9 1 6 1 3 2 5 9 8 1 1 4 3 2 2 1 1 4 8 3 4 2 4 1 7 1 1 1 7 5 3 1 3 1 9 1 6 1 9)
    #(2 5 1 4 6 6 9 1 5 2 1 9 1 2 6 4 4 2 9 1 2 2 5 2 7 5 5 4 9 1 5 6 2 1 5 2 5 2 3 2 9 8 2 2 1 4 1 2 7 9 2 2 5 9 1 1 2 4 6 1 7 4 2 1 2 2 7 9 8 6 1 9 2 1 7 2 4 6 4 5 2 1 1 7 1 5 5 4 1 1 3 2 9 3 2 4 1 2 5 5)
    #(1 7 7 4 4 1 6 8 9 7 4 3 9 3 6 4 2 4 1 3 1 1 5 2 5 5 1 8 9 6 3 3 2 6 1 2 1 8 2 5 4 6 1 4 9 1 8 3 4 1 3 5 9 1 7 3 1 5 6 7 8 1 4 5 4 4 6 3 9 4 2 2 2 1 1 9 2 5 2 4 1 5 1 8 1 5 5 7 2 1 2 3 1 9 9 7 9 8 3 6)
    #(2 9 2 4 4 1 8 1 7 1 2 1 2 1 6 6 3 7 6 2 1 7 3 3 2 2 1 9 9 2 8 5 2 4 1 2 5 1 6 1 1 5 1 3 8 1 3 5 2 2 8 3 9 8 1 2 4 9 2 1 9 5 3 6 8 1 1 1 8 9 5 9 3 1 9 1 1 9 1 5 9 9 8 2 8 1 5 3 9 1 2 1 2 4 7 7 1 1 5 3)
    #(4 7 2 7 2 8 4 7 5 7 4 1 7 2 3 8 2 2 1 1 9 5 5 7 1 7 2 7 1 4 3 8 1 6 9 3 2 5 1 9 9 1 2 3 3 2 1 6 4 1 9 1 1 1 1 3 1 3 5 6 2 5 8 2 9 3 1 7 6 9 3 9 1 7 7 3 9 6 3 5 5 5 1 3 2 6 3 1 2 6 9 4 2 1 6 4 2 1 1 2)
    #(1 8 2 6 1 7 6 1 3 2 9 3 2 5 9 1 2 1 1 8 1 1 2 8 1 4 9 4 1 8 4 1 2 4 6 4 9 6 7 1 1 4 4 9 3 5 5 5 1 5 3 6 2 2 2 1 2 9 6 7 5 1 3 2 7 3 2 1 1 2 3 1 4 3 3 3 1 3 1 8 8 3 1 7 3 9 2 8 2 1 6 1 2 4 3 4 3 1 3 6)
    #(3 9 4 1 7 9 1 8 9 1 1 2 5 2 7 9 1 1 8 4 6 5 1 2 8 3 8 1 5 9 1 1 4 1 7 1 4 9 3 6 4 1 2 1 1 1 1 4 4 8 7 2 1 4 2 1 9 6 3 5 9 8 5 6 9 5 3 1 2 5 1 1 6 8 1 5 5 8 2 1 3 9 6 1 8 1 2 6 2 3 8 1 1 9 1 1 5 1 9 4)
    #(6 3 8 4 1 1 7 1 5 2 7 7 1 5 1 4 9 7 9 5 5 2 4 7 1 4 4 2 8 8 1 9 4 1 7 5 5 3 9 8 7 2 2 4 4 6 8 8 6 7 9 1 4 1 3 2 3 9 1 1 4 5 6 1 6 7 1 9 3 1 4 8 2 1 2 6 1 2 3 8 9 5 1 8 3 7 2 2 9 3 3 1 2 3 6 5 3 3 8 6)
    #(8 2 8 8 1 7 1 1 8 2 9 7 9 8 8 3 4 6 7 2 5 1 1 6 8 2 1 1 8 4 5 9 2 6 3 2 5 3 2 8 5 4 8 7 7 9 1 1 2 5 2 2 1 3 4 1 2 2 5 5 6 2 9 2 3 4 4 8 3 9 6 5 6 3 9 2 4 3 3 1 2 6 1 1 4 2 9 6 5 4 6 5 7 6 9 6 9 1 1 1)
    #(2 1 3 8 9 8 9 1 9 1 1 2 1 1 7 3 1 9 8 1 1 4 3 1 5 2 3 2 5 2 5 3 8 3 5 9 1 4 8 5 3 7 5 3 3 9 3 6 3 4 1 9 5 1 4 3 5 2 5 1 7 5 7 3 2 2 1 1 2 1 1 7 1 9 3 3 4 1 3 2 5 4 9 2 1 4 1 8 5 1 3 7 4 2 6 3 1 8 3 1)
    #(2 2 1 3 9 3 7 5 2 1 2 1 9 1 1 1 5 5 6 4 2 4 7 1 7 1 1 2 8 1 3 9 1 8 8 5 9 8 1 1 6 1 3 1 4 9 2 1 7 5 4 2 1 2 1 2 5 8 7 7 1 3 2 5 6 8 5 4 4 6 2 9 9 1 1 3 4 3 6 2 4 3 9 2 4 3 2 9 1 1 1 8 8 3 4 3 4 5 5 9)
    #(5 6 1 7 4 1 1 2 1 2 7 2 9 9 1 4 4 9 4 1 8 2 2 1 9 7 7 3 3 1 6 9 3 2 2 8 1 9 1 9 1 5 5 5 9 3 2 7 7 1 3 1 4 2 3 8 5 9 4 4 8 6 2 6 5 2 1 7 2 2 1 1 7 8 7 3 7 2 8 6 9 4 5 4 9 8 2 3 5 4 1 9 1 2 3 1 2 2 3 1)
    #(7 2 2 1 1 1 8 9 7 2 1 1 1 4 2 1 3 9 1 1 3 4 7 2 9 2 9 2 8 3 7 3 2 1 2 8 1 5 7 6 3 1 2 9 8 6 5 6 3 1 1 2 9 7 7 4 5 8 4 6 7 3 1 9 4 1 8 6 3 9 2 8 2 5 7 4 6 4 9 2 4 3 1 1 7 2 7 1 3 7 9 4 9 9 8 6 7 9 1 6)
    #(7 4 8 1 3 8 1 6 8 1 2 1 4 1 1 3 3 2 9 8 3 9 1 1 8 5 1 8 9 9 3 9 1 1 9 8 7 1 9 9 7 4 3 4 4 9 9 2 9 1 1 9 4 9 7 1 4 1 2 2 9 9 3 1 3 1 7 3 1 5 3 4 7 9 5 3 9 5 8 7 6 1 5 5 8 4 2 3 8 1 8 1 4 4 7 1 4 9 1 4)
    #(1 9 6 2 8 2 8 6 2 1 4 2 1 9 1 2 1 2 7 1 9 5 2 4 5 9 3 8 4 2 4 1 2 1 7 5 4 6 9 7 1 1 8 6 4 2 1 9 4 2 3 2 4 1 4 8 2 7 8 4 8 9 1 2 3 1 8 1 3 1 7 2 2 9 6 9 6 1 1 1 9 2 5 4 1 6 1 4 7 1 2 4 1 3 1 2 4 3 8 9)
    #(1 4 5 3 4 1 4 9 1 6 6 6 1 1 1 8 7 9 6 1 9 3 1 2 3 1 8 9 1 6 9 6 2 7 9 3 4 9 4 3 1 9 5 1 3 1 5 8 7 1 2 7 1 3 5 1 1 1 4 4 1 9 8 8 9 2 1 2 1 5 1 2 1 9 7 1 4 6 1 3 2 3 9 9 5 3 1 1 7 6 1 1 9 2 2 5 4 7 7 8)
    #(1 5 1 8 3 2 3 1 5 4 6 3 9 9 1 1 2 2 2 8 8 6 5 1 4 1 1 1 8 9 3 8 5 1 3 5 9 2 3 5 1 9 2 3 9 2 2 1 1 1 1 5 7 4 1 1 3 5 9 8 4 9 1 9 8 1 1 5 5 3 4 4 3 9 4 5 1 9 1 5 2 4 4 2 1 1 1 2 2 7 1 1 4 9 3 5 1 1 6 5)
    #(2 1 2 4 6 1 9 1 1 8 9 2 8 8 1 2 4 8 8 8 3 2 4 1 2 3 1 9 3 7 8 2 2 2 6 2 2 4 4 1 9 2 7 3 3 2 9 2 5 5 8 7 4 9 1 8 1 4 1 3 3 3 6 7 1 3 9 2 2 8 5 1 9 2 1 9 1 5 9 5 1 2 1 5 9 7 8 1 8 3 9 6 6 1 3 9 1 2 5 2)
    #(9 1 3 6 1 2 3 2 3 8 7 1 3 3 9 1 2 1 3 6 1 5 3 3 8 8 3 6 8 1 4 1 7 3 6 1 3 3 7 2 1 9 2 3 3 7 5 1 2 8 9 1 8 8 1 4 7 9 1 9 5 9 8 8 5 2 1 9 3 6 9 1 5 1 1 9 1 1 9 4 3 5 8 1 2 2 8 4 1 1 5 1 3 4 9 8 5 1 2 1)
    #(3 1 8 7 2 6 1 9 8 5 3 1 3 1 1 8 1 1 6 9 7 2 5 7 1 8 2 1 2 1 2 9 1 3 9 1 1 9 6 3 7 1 4 6 3 1 1 2 6 7 3 1 2 2 1 2 6 6 3 1 4 5 2 9 1 9 6 5 4 1 1 1 9 3 2 2 5 7 1 1 3 7 3 1 2 9 9 5 7 4 4 9 2 3 6 3 1 5 4 4)
    #(2 3 3 2 1 9 6 7 7 1 1 8 1 4 2 2 1 2 1 4 7 1 6 1 1 1 3 3 2 5 9 3 2 1 3 1 3 1 3 6 9 4 7 1 4 1 2 1 7 6 2 4 1 1 3 3 1 3 1 8 1 1 8 9 1 8 2 8 9 9 5 7 1 6 1 3 2 2 9 6 1 1 5 1 1 5 2 9 1 3 4 3 9 2 8 4 6 3 4 1)
    #(2 2 5 1 1 1 9 4 2 5 2 1 2 2 7 7 1 5 1 2 1 5 2 9 1 7 8 1 1 8 9 3 1 4 9 7 5 2 2 1 7 1 9 4 1 3 1 3 4 9 6 7 6 6 5 3 1 1 8 6 9 1 1 4 5 2 1 7 2 9 4 9 9 1 2 1 1 1 6 2 1 3 8 8 5 1 6 3 7 4 6 1 1 1 5 3 5 1 3 7)
    #(9 5 8 6 2 2 6 1 9 9 4 1 7 3 1 7 1 5 1 9 1 5 2 8 2 2 9 1 1 4 9 9 9 5 6 3 1 7 8 1 7 3 1 8 5 1 1 5 9 7 1 1 3 4 8 6 1 1 2 1 2 6 2 8 4 9 3 2 7 1 2 6 3 1 5 2 5 5 5 2 7 3 2 1 3 3 1 6 7 7 6 4 9 2 7 9 3 6 5 8)
    #(9 9 1 2 6 2 6 8 4 1 1 2 9 2 5 1 2 1 9 1 5 1 2 1 7 8 3 3 9 1 2 9 7 3 3 1 1 2 2 3 7 7 2 8 1 4 4 9 6 5 9 3 2 9 7 4 7 1 7 3 1 3 5 9 8 1 8 9 9 1 6 9 9 1 4 7 9 8 9 9 2 8 2 4 3 8 9 4 3 1 3 2 4 7 5 3 2 2 3 1)
    #(3 4 1 8 2 4 1 6 1 8 9 4 7 5 1 6 3 3 2 2 2 7 6 1 3 8 3 1 3 7 4 9 2 1 8 7 4 8 5 5 5 8 4 1 2 5 5 5 8 1 1 4 4 2 9 4 7 1 1 2 1 3 2 1 2 3 2 1 7 7 2 1 4 3 2 1 5 9 8 5 2 1 5 9 7 1 6 1 1 9 1 1 2 8 2 1 6 9 5 4)
    #(8 8 2 2 6 1 6 1 2 3 2 7 4 8 6 1 4 1 1 7 6 9 1 2 8 2 1 3 4 2 1 2 3 2 1 1 3 1 7 5 7 9 2 1 7 8 7 1 3 1 5 3 3 5 1 7 4 4 8 5 6 5 2 1 3 9 6 1 2 8 1 1 7 1 4 7 1 8 2 7 1 2 4 4 3 4 1 1 5 1 2 1 9 8 5 4 7 6 2 1)
    #(5 1 3 6 3 3 3 3 9 1 4 1 4 8 1 2 8 4 2 1 3 9 9 9 9 6 2 7 6 1 2 2 5 1 1 8 1 1 1 9 1 1 6 1 3 4 1 1 5 9 9 3 2 3 7 2 2 1 1 9 4 4 3 2 7 7 4 3 2 7 4 6 4 9 2 6 1 3 5 1 8 1 5 1 9 8 1 9 1 5 7 2 4 3 1 5 7 3 1 1)
    #(3 2 5 1 7 1 2 3 8 4 2 2 1 2 2 9 3 3 9 1 1 1 1 9 7 2 3 9 3 6 5 1 1 4 1 8 9 1 7 6 8 1 9 9 5 2 3 8 4 1 2 5 8 3 1 1 1 1 9 7 5 1 4 8 5 7 1 1 8 4 6 5 2 3 8 7 1 2 1 2 5 7 4 8 1 1 3 3 3 1 1 9 8 1 2 3 8 5 1 4)
    #(4 1 7 9 2 2 1 8 9 7 2 5 3 1 9 6 9 4 3 1 1 1 4 4 4 9 9 4 3 8 1 4 5 2 9 9 8 2 3 2 7 7 9 7 5 9 9 4 7 1 1 9 7 1 4 5 2 9 1 3 7 5 4 1 9 6 4 4 1 1 1 5 4 3 4 1 8 5 1 4 1 2 4 6 8 9 6 6 6 1 2 1 2 8 5 4 6 5 7 2)
    #(3 5 6 1 5 1 1 8 6 1 9 7 6 9 1 5 2 8 8 8 1 9 1 3 6 2 3 7 4 7 1 5 6 9 1 9 9 9 9 1 2 2 1 8 1 1 1 2 8 6 1 1 5 9 5 2 9 9 1 2 1 1 1 7 9 4 2 3 2 9 6 2 1 1 2 4 9 1 1 1 9 4 5 1 1 4 8 4 2 5 9 3 1 7 2 1 1 3 3 9)
    #(7 1 1 1 1 2 9 2 2 9 7 3 1 2 6 1 1 1 1 2 8 3 4 1 5 3 9 3 2 9 6 1 1 3 7 7 3 3 6 4 2 8 7 1 9 1 6 8 1 1 1 5 1 1 3 2 1 8 8 2 5 8 1 3 2 1 3 4 8 7 8 2 8 2 4 9 2 2 2 9 6 2 6 9 5 1 1 5 5 1 5 6 9 1 8 7 3 5 1 4)
    #(7 3 2 9 4 1 1 1 8 2 3 4 3 1 4 8 3 3 7 1 3 3 4 6 1 1 4 4 1 8 1 9 3 1 2 1 9 1 1 8 2 7 9 3 3 6 6 3 8 6 8 8 6 4 3 3 6 6 2 7 1 8 2 1 2 5 5 1 7 1 2 2 4 1 4 7 6 4 1 1 9 8 1 8 1 9 1 8 2 3 6 5 2 2 3 6 7 6 4 1)
    #(9 2 8 3 2 7 3 1 1 4 2 4 1 1 9 7 5 3 2 1 6 8 1 4 9 3 2 6 9 7 7 4 4 2 9 7 2 1 3 2 1 3 4 2 1 2 1 1 6 8 4 5 5 8 2 2 7 9 6 8 1 5 6 3 7 7 2 6 7 7 1 2 2 9 3 1 5 7 1 2 3 9 5 3 5 2 2 6 5 8 1 8 1 9 8 4 3 5 2 9)
    #(1 9 9 8 8 2 6 2 1 1 2 4 3 4 6 5 1 4 2 3 1 4 1 7 1 4 9 4 1 1 5 6 3 8 9 4 7 3 1 2 3 9 7 5 8 9 3 6 2 1 9 3 7 1 1 3 8 5 1 7 9 6 1 9 2 1 6 3 3 2 3 1 1 1 9 5 1 9 2 9 1 9 7 9 7 7 6 6 4 2 3 1 1 1 7 6 9 1 7 1)
    #(5 5 2 5 1 1 2 2 9 4 5 9 1 8 1 8 2 6 7 9 1 1 3 8 9 1 6 2 2 1 1 1 2 3 2 2 8 6 1 1 2 4 1 3 7 2 1 1 1 4 8 7 5 7 1 1 1 1 1 1 4 3 2 2 1 3 2 7 4 1 6 1 2 1 4 1 2 1 3 8 5 3 1 3 7 2 8 9 1 1 2 2 8 2 1 4 5 3 6 9)
    #(3 7 4 9 1 1 2 3 4 9 3 7 9 7 8 8 1 1 9 3 7 2 8 5 1 5 7 2 6 9 4 9 7 2 2 5 5 2 2 1 1 4 1 1 7 2 8 3 8 2 1 3 5 5 7 1 1 1 1 9 6 1 1 7 2 5 5 9 3 2 7 8 7 2 6 6 7 3 9 8 8 5 2 1 9 6 6 7 1 1 5 5 9 5 8 7 8 1 1 5)
    #(2 7 1 1 3 5 3 2 1 1 4 7 2 1 2 1 9 8 9 3 7 2 1 2 4 1 8 2 6 7 8 1 1 7 3 9 8 9 1 7 8 1 1 2 3 9 2 1 9 1 5 6 3 1 5 9 5 3 1 9 3 6 2 8 3 4 4 5 5 2 1 6 3 4 5 1 1 6 1 9 1 7 4 1 9 1 8 3 1 1 4 1 9 9 8 9 7 1 7 2)
    ) ) 





(define (onboard vec x y)
  (and (>= x 0)
       (>= y 0)
       (< y (vector-length vec))
       (< x (vector-length (vector-ref vec 0)))))

(define (get-xy vec x y)
  (when (not (onboard vec x y))
    (format #t "get-xy : failed assertion ~%")
    (format #t "onboard x y vec : ~a ~a ~a ~%" x y vec)
    (error (list "get-xy  failure")))
  (vector-ref (vector-ref vec y) x))

(define (set-xy vec x y z)
  (when (not (onboard vec x y))
    (format #t "set-xy : failed assertion ~%")
    (format #t "onboard x y vec : ~a ~a ~a ~%" x y vec)
    (error (list "set-xy  failure")))
  (vector-set! (vector-ref vec y) x z))

(define (get-width vec)
  (vector-length (vector-ref vec 0)))

(define (get-height vec)
  (vector-length vec))

(define (deep-copy vec)
  (let ((width (get-width vec))
	(height (get-height vec)))
    (let ((res (make-vector height #f)))
      (do-list (h (iota height))
	       (vector-set! res h (make-vector width #f)))
      res)))


(define (top-right vec)
  (let ((tot 0)
	(wid (get-width vec))
	(hgt (get-height vec)))
    ;; top .... right 
    (do-list (y '(0))
      (do-list (x (iota wid))
	;;(format #t "alpha : x = ~a : y = ~a~%"   x  y)
	(set! tot (+ tot (get-xy vec x y)))))
    (do-list (y (cdr (iota hgt)))
      (do-list (x (list (1- wid)))
	;;(format #t "beta : x = ~a : y = ~a~%"   x  y)
	(set! tot (+ tot (get-xy vec x y)))))
    tot))



(define (bot-left vec)
  (let ((tot 0)
	(wid (get-width vec))
	(hgt (get-height vec)))
    ;; left side ... bottom row 
    (do-list (y (iota hgt))
      (do-list (x '(0))
	;;(format #t "alpha : x = ~a : y = ~a~%"   x  y)
	(set! tot (+ tot (get-xy vec x y)))))
    (do-list (y (list (1- hgt)))
      (do-list (x (cdr (iota wid)))
	;;(format #t "beta : x = ~a : y = ~a~%"   x  y)
	(set! tot (+ tot (get-xy vec x y)))))
    tot))



#|
;; #;1> (sum-tot input)
;;41431
;; 41431...100....100  encoded as
;; <sum> ... <x coord> ... <y coord> ...
;;
this just adds up all the numbers in 2d array
|#
(define (sum-tot vec)
  (let ((tot 0)
	(wid (get-width vec))
	(hgt (get-height vec)))
    (do-list (y (iota hgt))
      (do-list (x (iota wid))
	       (set! tot (+ tot (get-xy vec x y)))))
    tot))



(define (run vec goal guide)
  (let* ((width (get-width vec))
	 (height (get-height vec))
	 (wid-1 (- width 1))
	 (hgt-1 (- height 1))
	 (best-path '())
	 (best-cost goal)
	 (hist (deep-copy vec))
	 (solutions 0)
	 )
    
    (define (seek x y cost path)
      (define (left)
	(when (> x 0)
	  (let* ((x2 (1- x))
		 (y2 y)
		 (cost2 (+ cost (get-xy vec x2 y2)))
		 (path2 path) ;;(cons (list x2 y2) path)))
		 )

	    (when (not (get-xy hist x2 y2))
	      (set-xy hist x2 y2 #t)
	      (seek x2 y2 cost2 path2)
	      (set-xy hist x2 y2 #f)
	      )

	    )))
      (define (up)
	(when (> y 0)
	  (let* ((x2 x)
		 (y2 (1- y))
		 (cost2 (+ cost (get-xy vec x2 y2)))
		 (path2 path) ;;(cons (list x2 y2) path)))
		 )
	    (when (not (get-xy hist x2 y2))
	      (set-xy hist x2 y2 #t)
	      (seek x2 y2 cost2 path2)
	      (set-xy hist x2 y2 #f)
	      )
	    
	    )))
      (define (down)
	(when (< y hgt-1)
	  (let* ((x2 x)
		 (y2 (1+ y))
		 (cost2 (+ cost (get-xy vec x2 y2)))
		 (path2 path) ;;(cons (list x2 y2) path)))
		 )
	    (when (not (get-xy hist x2 y2))
	      (set-xy hist x2 y2 #t)
	      (seek x2 y2 cost2 path2)
	      (set-xy hist x2 y2 #f)
	      )
	    
	    )))
      (define (right)
	(when (< x wid-1)
	  (let* ((x2 (1+ x))
		 (y2 y)
		 (cost2 (+ cost (get-xy vec x2 y2)))
		 (path2 path);;(cons (list x2 y2) path)))
		 )
	    (when (not (get-xy hist x2 y2))
	      (set-xy hist x2 y2 #t)
	      (seek x2 y2 cost2 path2)
	      (set-xy hist x2 y2 #f)
	      )
	    
	    )))
      (cond
       ((> cost best-cost) #f)
       ((and (= x wid-1) (= y hgt-1))
	(cond
	 ((<= cost best-cost)
	  (set! best-cost cost)
	  (set! best-path path)
	  (set! solutions (1+ solutions))
	  (format #t "new best solution ~a ~%" best-cost))
	 (#t
	  #f)))
	  ;;(format #t "any solution ~a ~%" cost))))
       ((eq? guide 'no-left-no-up)
	(right)
	(down))
       ((eq? guide 'no-left-no-up-random)
	(let ((r (pseudo-random-integer 2)))
	  (cond
	   ((= r 0) (right) (down))
	   (#t (down) (right)))))
       (#t
	(left)
	(up)
	(down)
	(right)
	)))
    
    (define (start)
      ;; clear history paths
      (do-for x (0 width)
	      (do-for y (0 height)
		      (set-xy hist x y #f)))
      (set-xy hist 0 0 #t)
      (seek 0 0 0 '((0 0)))
      (if (zero? solutions)
	  #f
	  solutions))

    (format #t "initially best-cost from edges is ~a ~%" best-cost)
    (start)))





(define (example-1)
  (run example))

(define (part-1 n)
  (call/cc (lambda (exit)
	     (let ((goal n))
	       (do-while #t			 
			 (format #t "trying goal with ~a ~%" goal)
			 (let ((r (time (run input goal 'all))))
			   (if r
			       (exit goal)
			       (set! goal (1+ goal)))))))))




(define (part-1b)
  (call/cc (lambda (exit)
	     (let ((goal 100))
	       (do-while #t
			 (set! goal (+ 10 goal))
			 (format #t "1b : trying goal with ~a ~%" goal)
			 (let ((r (time (run input goal 'no-left-no-up))))
			   (if r
			       (exit goal)
			       #f)))))))

;; 800 ...... see if we can capture output correctly
;; 752 ..... single process 100%
;; 682 ..... multiple random processes no comms

(define (part-1c)
  (let ((goal 682))
    (let ((r (time (run input goal 'no-left-no-up-random))))
      r)))



;;(example-1)

;; try just going right or down
;;(part-1b)

;;(part-1c)


#|

breadth first search

discontinue interest in a square if it returns me to a square with a greater cost than is already
at that square


|#



;;(define vec example)
;;(define vec input)
;;(define vec input)

(define (make-huge-five-o vec)
  (let* ((wid (get-width vec))
	 (hgt (get-height vec))
	 (big (make-vector (* hgt 5))))
    ;; rebuild 2d array
    (do-for e (0 (* hgt 5))
	    (vector-set! big e (make-vector (* wid 5) 0)))

    #|
    (when (not (vector? big))
      (format #t "big is not a vector ??"))
    (when (not (= 500 (get-width big)))
      (format #t "big error , wrong width!"))
    (when (not (= 500 (get-height big)))
      (format #t "big error , wrong height!"))
    |#

    
    (let ((n 0)
	  (ix 0)
	  (iy 0))
      ;;(format #t "~%")
      (do-for y (0 (* hgt 5) hgt)
	      (do-for x (0 (* wid 5) wid)
		      ;;...........................
		      ;;(format #t "a+~a " n)
		      (do-for iy (0 hgt)
			      (do-for ix (0 wid)
				      (let* ((val (get-xy vec ix iy))
					     (val2 (+ n val)))
					(when (> val2 9)
					  (set! val2 (+ 1 (modulo val2 10) )))

					;; (format #t "(~a,~a) [~a] => (~a,~a) [~a] ~%"
					;;       ix iy val
					;;       (+ ix x) (+ iy y) val2)
				      
				      (set-xy big (+ ix x) (+ iy y) val2)
				      )))
		      ;; ..............................		      
		      (set! n (+ n 1))
		      )
	      (set! n (- n 4))
	      ;;(format #t "~%")
	      ))
    big))


(define (expand-ok)
  (equal? example2 (make-huge-five-o example)))

(define (check-expand v tar)
  (let ((big (make-huge-five-o v)))     
    (let ((wid (get-width big))
	  (hgt (get-height big)))
      (do-for x (0 wid)
	      (do-for y (0 hgt)
		      (let ((a (get-xy big x y))
			    (b (get-xy tar x y)))
			(when (not (= a b))
			  (format #t "mis-match at ~a ~a : got ~a : expected ~a ~%" x y a b))))))))




;;(set! vec big)

;; expand 5 times and adjust accordingly
;; increment according to add 1 to left tile etc...
;;(set! vec (make-huge-five-o vec))



(define (solve vec)


(define hist (deep-copy vec))
;; make (0,0) known as value of 0
(set-xy hist 0 0 0)

(define wid (get-width vec))
(define hgt (get-height vec))

;; initial state is 
(define states (list (list 0 0 0)))
(define new-states 0)
(define res '())

(define (onboard? x y)
  (and (>= x 0)(< x wid)
       (>= y 0)(< y hgt)))

(define (direction x2 y2 c)
  (when (onboard? x2 y2)
    (let ((cost2 (+ c (get-xy vec x2 y2)))
	  (maybe (get-xy hist x2 y2)))
      (cond
       ((and (integer? maybe) (< cost2 maybe))	
	(set! res (cons (list x2 y2 cost2) res))
	(set! new-states (+ 1 new-states))
	(set-xy hist x2 y2 cost2))
       ((not maybe)
	(set-xy hist x2 y2 cost2)
	(set! new-states (+ 1 new-states))
	(set! res (cons (list x2 y2 cost2) res)))
       ((integer? maybe) #f)
       (#t (error 'direction 'fault (list 'maybe maybe)))))))



(define (right x y c)
  (let ((x2 (+ x 1))(y2 y))
    (direction x2 y2 c)))

(define (left x y c)
  (let ((x2 (- x 1))(y2 y))
    (direction x2 y2 c)))
    
(define (up x y c)
  (let ((x2 x)(y2 (- y 1)))
    (direction x2 y2 c)))

(define (down x y c)
  (let ((x2 x)(y2 (+ y 1)))
    (direction x2 y2 c)))
    
(define (reach s)
  (let ((x (first s))
	(y (second s))
	(c (third s)))
    (right x y c)
    (left x y c)
    (up x y c)
    (down x y c)))

	
(define (breadth)
  (set! new-states 0)
  (do-list (s states)
	   (reach s))
  (set! states res)
  (set! res '())
  (format #t "generated ~a new states~%" new-states))

(define (run)
  (breadth)
  ;;(pp hist)
  (format #t "~%~%")
  (when (> new-states 0)
    (run)))




(define (show)
  (run)
  (format #t "~%~%")
  ;;(pp hist)
  (format #t "corner value bottom left is ~a ~%" (get-xy hist (- wid 1) (- hgt 1)))
  (format #t "~%~%"))

(show)

)


;; pose problem to system
;;(solve big)

;;(solve (make-huge-five-o example))

;;(equal? (make-huge-five-o example) example2)

		      
(solve (make-huge-five-o input))





#|

    403
    403))

real	0m0.082s
user	0m0.065s
sys	0m0.017s
[terry@terry-allseries day15]$ 

suggested answer is 403


          0..   100.. 200..  300.. 400..
-------------------------------------------
0..       a     a+1   a+2   a+3   a+4
100...    a+1   a+2   a+3   a+4   a+5
200..     a+2   a+3   a+4   a+5   a+6
300..     a+3   a+4   a+5   a+6   a+7
400..     a+4   a+5   a+6   a+7   a+8



..........................................
computations proceeds ..................
...........................................




after checking expansion / tiling of input to larger input
against example / example2 ok


real	0m3.384s
user	0m3.338s
sys	0m0.040s


corner value bottom left is 2840 

congrats
accepted




|#










  











