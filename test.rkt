#lang racket
(require "asm.rkt")
(require threading)
(require "core-lib.rkt")
(require syntax/parse/define)
(require (for-syntax racket/list))


(scurry
 (import core-lib)
 (def x
   (list
    (create-obj (["A" 1]))
    (create-obj (["A" 2]))
    (create-obj (["A" 3]))))

 (def o (create-obj (["A" (list 1 2 3 4)])))
 

 (def x (~ split-top 2 (get-prop o "A")))



 (assert (~ lists-same? x (list 3 4 4)) "list " x " not equal to 3 4 4")
 (assert (~ lists-same? x (list 3 2)) "list " x " not equal to 3 2")
 (assert (~ lists-same? x (list 3 4)) "list " x " not equal to 3 2")

 (def y (~ split-bottom 2 (list 1 2 3 4)))
 (assert (~ lists-same? y (list 1 2))
         "take-bottom broken: " y)

 
 ;; (dbgl x)
 ;; (sortByDesc x "A")
 ;; (dbgl x)
 ;; (sortDesc x)
; (dbgl x)


 )

