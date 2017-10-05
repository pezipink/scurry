#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)
(scurry
 (import core-lib)
 ; list tests

 ; split-top removes n elements and creates
 ; a new list from them
 (def x (list 1 2 3 4 5 6 7 8)) 
 (def y (split-top 3 x))
 (assert (lists-same? (list 1 2 3 4 5) x)
         "expected [1 2 3 4 5] but got " x)
 (assert (lists-same? (list 6 7 8) y)
         "expected [6 7 8] but got " y)

 ; split-bottom removes n elements from the
 ; front, creating a new list from them
 (def z (split-bottom 2 x))
 (assert (lists-same? (list 3 4 5) x)
         "expected [3 4 5] but got " x)
 (assert (lists-same? (list 1 2) z)
         "expected [1 2] but got " z)
 
 ; remove-list removes a single element

 (remove-list x 4)
 (assert (lists-same? (list 3 5) x)
         "expected [3 5] but got " x)

 (global.hello <- "world")

 (dbgl "hello global " global.hello)
 
 (def-obj function-table
   (["f" (λ (return _))]
    ["square" (λ (return (add _ _)))]
    ["cube" (λ (return (add _ _ _)))]
    ["twice" (λ (f x) (return (f (f x))))]
    ["juan" (list 1 2 3)]
    ))

 
 (dbgl "10 squared :" (function-table.square 10))
 (dbgl "10 cubed :" (function-table.cube 10))
 (dbgl "10 squared squared :"
       (function-table.twice function-table.square 10))



 (def-obj override-functions
   (["square2" (λ (begin
                   (dbgl "inside debug square with " _)
                   (return (add _ _))))]))


 ;how to handle things that modify or override some function
 ;for example, it usually takes 

 

 (def-λ (virtual-dispatch object default-object function-name arg)
   (if (contains  object function-name)
       ;call speciailised method
       ((get-prop object function-name) arg)
       ;call default
       ((get-prop default-object function-name) arg)))
   

 (dbgl "res : "
       (virtual-dispatch
        override-functions
        function-table
        "square" 10))


 'brk
 (def obj2 (clone function-table))
 (obj2.test <- "hello")

 (when (obj2.test = "hello")
   (dbgl "!"))
 
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2) 

 (append-list obj2.juan "88")
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2) 
 
 'brk
 (dbgl "end")
)
