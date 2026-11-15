

(defpackage :day16
  (:use :cl))

(in-package :day16)

;; this is common lisp
(+ 1 2 )
(defun hi () "hello world!")

(defparameter input "E20D7880532D4E551A5791BD7B8C964C1548CB3EC1FCA41CC00C6D50024400C202A65C00C20257C008AF70024C00810039C00C3002D400A300258040F200D6040093002CC0084003FA52DB8134DE620EC01DECC4C8A5B55E204B6610189F87BDD3B30052C01493E2DC9F1724B3C1F8DC801E249E8D66C564715589BCCF08B23CA1A00039D35FD6AC5727801500260B8801F253D467BFF99C40182004223B4458D2600E42C82D07CC01D83F0521C180273D5C8EE802B29F7C9DA1DCACD1D802469FF57558D6A65372113005E4DB25CF8C0209B329D0D996C92605009A637D299AEF06622CE4F1D7560141A52BC6D91C73CD732153BF862F39BA49E6BA8C438C010E009AA6B75EF7EE53BBAC244933A48600B025AD7C074FEB901599A49808008398142013426BD06FA00D540010C87F0CA29880370E21D42294A6E3BCF0A080324A006824E3FCBE4A782E7F356A5006A587A56D3699CF2F4FD6DF60862600BF802F25B4E96BDD26049802333EB7DDB401795FC36BD26A860094E176006A0200FC4B8790B4001098A50A61748D2DEDDF4C6200F4B6FE1F1665BED44015ACC055802B23BD87C8EF61E600B4D6BAD5800AA4E5C8672E4E401D0CC89F802D298F6A317894C7B518BE4772013C2803710004261EC318B800084C7288509E56FD6430052482340128FB37286F9194EE3D31FA43BACAF2802B12A7B83E4017E4E755E801A2942A9FCE757093005A6D1F803561007A17C3B8EE0008442085D1E8C0109E3BC00CDE4BFED737A90DC97FDAE6F521B97B4619BE17CC01D94489E1C9623000F924A7C8C77EA61E6679F7398159DE7D84C015A0040670765D5A52D060200C92801CA8A531194E98DA3CCF8C8C017C00416703665A2141008CF34EF8019A080390962841C1007217C5587E60164F81C9A5CE0E4AA549223002E32BDCEA36B2E100A160008747D8B705C001098DB13A388803F1AE304600")

;; convert to boolean array 
(defparameter input-array (make-array (* 4 (length input)) :initial-element 0))
;; fill the array
(defun fill-array ()
  (let ((ix 0))
    (labels ((fill-bits (a b c d)
               (setf (aref input-array (+ ix 0)) a)
               (setf (aref input-array (+ ix 1)) b)
               (setf (aref input-array (+ ix 2)) c)
               (setf (aref input-array (+ ix 3)) d)
               (incf ix 4)))
  (loop for i from 1 to (length input) do ;;cx = character index in input string
        (let* ((cx (- i 1))
               (ch (aref input cx)))
          (cond
            ((char= ch #\0) (fill-bits 0 0 0 0))
            ((char= ch #\1) (fill-bits 0 0 0 1))
            ((char= ch #\2) (fill-bits 0 0 1 0))
            ((char= ch #\3) (fill-bits 0 0 1 1))
            ((char= ch #\4) (fill-bits 0 1 0 0))
            ((char= ch #\5) (fill-bits 0 1 0 1))
            ((char= ch #\6) (fill-bits 0 1 1 0))
            ((char= ch #\7) (fill-bits 0 1 1 1))
            ((char= ch #\8) (fill-bits 1 0 0 0))
            ((char= ch #\9) (fill-bits 1 0 0 1))
            ((char= ch #\A) (fill-bits 1 0 1 0))
            ((char= ch #\B) (fill-bits 1 0 1 1))
            ((char= ch #\C) (fill-bits 1 1 0 0))
            ((char= ch #\D) (fill-bits 1 1 0 1))
            ((char= ch #\E) (fill-bits 1 1 1 0))
            ((char= ch #\F) (fill-bits 1 1 1 1))
            (t (error "bad fill bits"))))))))

;; destructively modify input-array
(fill-array)            
;; now input-array is a box of bits either 1 or 0 

;; can we convert a boolean string into a decimal number 001 = 1 , 100 = 4 etc .. 101 = 5 
(parse-integer "010" :radix 2)

#| 
packet parsing
we need to keep track of how many bits are read since they are groups of four bits

packet 
VERSION 3 bits
TYPEID  3 bits -- 

TYPEID == 4 then parse like this as binary number
TYPEID != 4 then parse like operator 

LENGTHID == 0 next 15 bits determine number of sub packets
LENGTHID == 1 next 11 bits determine number of sub packets

straight after the sub packets appear 

inherently a recursive problem ... so cant have a global ir ... 
unless ? we could stack potential positions of the sub packets maybe ?


|#

(defparameter ir 0) ;; index reader
(defun reset () 
  (setq ir 0))

(reset)

(defun packlit ()  
  ;; consumed 6 bits so far
  (format t "packlit found."))

(defun pack15 ()
  ;; operator consumed 7 bits so far , now 15 bits extra for number sub packets
  (format t "pack15 found."))

(defun pack11()
  ;; operator consumed 7 bits so far , now 11 bits extra for number sub packets
  ;; 11 10   9 8 7 6 5 4 3 2 1 
  ;; ir ir+1 2 3 4 5 6 7 8 9 10
  (format t "pack11 found.")
  (let* ((version (+ (* (exp 2 0) (aref input-array (+ 2 ir))) ;; 0 1 2 ... 3 4 5
                     (* 2 (aref input-array (+ 1 ir)))
                     (* 4 (aref input-array (+ 0 ir)))))
  
)


(defun packrat ()  
  (let* ((version (+ (* 1 (aref input-array (+ 2 ir))) ;; 0 1 2 ... 3 4 5
                     (* 2 (aref input-array (+ 1 ir)))
                     (* 4 (aref input-array (+ 0 ir)))))
         (type    (+ (* 1 (aref input-array (+ 5 ir)))
                     (* 2 (aref input-array (+ 4 ir)))
                     (* 4 (aref input-array (+ 3 ir))))))                     
    (incf ir 3);;ignore 3 version bits
    (incf ir 3);;ignore 3 type bits
    (format t "version ~a type ~a ~%" version type)
    (cond
      ((= type 4) (packlit))
      (t 
       (incf ir) ;; length id 
       (let* ((len (aref input-array (+ 6 ir))))
         (cond
           ((= len 0) (pack15))
           ((= len 1) (pack11))
           (t (error "packrat"))))))))



  
