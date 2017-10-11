#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)

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
    ["juan" (list 1 2 3)]))

 'brk
 
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
 
 (def mut 10)
 (set-var mut 20)
 (assert (mut = 20))
 
 (def-λ (mutation-test)
   ; mutating outside of closure scope
   (mut <- 30)
   (assert (mut = 30))
   ; shadow
   (def mut 50)
   (assert (mut = 50)))

 (mutation-test)
 (assert (mut = 30))

 (def-λ (mutation-test byval)
   (assert (mut = 30))
   (byval <- 40)
   (assert (mut = 30))
   (assert (byval = 40)))
 
 (mutation-test mut)
 
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


 (def obj2 (clone-obj function-table))
 (obj2.test <- "hello")
 
 (assert (obj2.test = "hello"))
 
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2) 

 (append-list obj2.juan "88")
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2)

 ;test closures and let scoping
 (def-λ (test-closure n)
   (let ([n2 7]
         [f (λ (x y)
              (begin
                (def ret (add n n2 x y))
                ; mutation inside closure
                (n2 += 1)
                (return ret)))])
     (return f)))

 (def test-closure-f (test-closure 37))
 (assert (eq (test-closure-f 10 20) 74))
 (assert (eq (test-closure-f 10 20) 75))

 (def-obj props-obj
   (["n" 10]
    ["prop1" (create-obj
             (["prop2" 42]))]))

 (props-obj.n += 10)
 (assert (props-obj.n = 20))
 (assert (props-obj.prop1.prop2 = 42))
 (props-obj.prop1.prop2 <- 58)
 (assert (props-obj.prop1.prop2 = 58))
 (props-obj.prop1.prop2 ++)
 (assert (props-obj.prop1.prop2 = 59))

 (props-obj.prop1.prop4 <- (create-obj (["final" (create-obj (["n" 10]))])))
 (assert (props-obj.prop1.prop4.final.n = 10))
 (props-obj.prop1.prop4.final.n --)
 (assert (props-obj.prop1.prop4.final.n = 9))

 ; is- tests
 (assert (is-list? (list)))
 (assert (not (is-list? 10)))
 (assert (is-bool? #t))
 (assert (not  (is-bool? (list))))
 (assert (is-obj? props-obj))


 'brk
 'brk

 (dbgl "end"))


