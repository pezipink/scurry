#lang racket
(require "asm.rkt")
(require threading)
(require "core-lib.rkt")
(require syntax/parse/define)
(require (for-syntax racket/list))



(scurry
 (def x 10)
; (import core-lib)
 
 ;; (def-obj x (["numbers" (s-list 1 2 3 4 5)]
 ;;             ["forty-two" 42]
 ;;             ["add-ten" (λ (add _ 10))]))

 ;; (extract x (numbers forty-two add-ten)          
 ;;   (dbgl (~ map add-ten numbers))
 ;;   (append-list numbers 6 7 8 9 10 forty-two)
 ;;   (dbgl (~ sum numbers)))

 ;; output
 ;; [11, 12, 13, 14, 15]
 ;; 97

 ; (dbgl (~ sum nums))
  ;; ;create list of events
 ;; (set-global "on-juan" '(createlist))

 ;; ; function to raise events
 ;; (def-λ (raise-event event-handlers event-arg)
 ;;   (foreach (h event-handlers)
 ;;      (~ h event-arg)))

 ;; ;add some events
 ;; (~ append-global-list "on-juan" (λ (dbgl _)))
 ;; (~ append-global-list "on-juan" (λ (dbgl (add _ 42))))
 
 ;; (def numbers (s-list 1 2 3 4 5))
 ;; (def some-number 42)

 ;; (def (x y) (tuple 10 20))


 ;; (def t (tuple 10 20))

 ;; (def (a b) t)
 
 ;; (def-λ (tuple-test ((x y)))
 ;;   (dbgl " X " x " Y " y))
 

 ;; (dbgl "1TUPLE " x " " y)

 ;; (~ tuple-test t)
 
 ;; (foreach (n numbers)
 ;;   (~ raise-event (get-global "on-juan") n))
  
 ;; (~>> 
 ;;  ;pipeline
 ;;  numbers
 ;;  (~ filter (λ (gt _ 3)))
 ;;  (~ map    (λ (add _ some-number)))
 ;;  (dbgl))

 ;;  (def x (~ partition (λ (gt _ 3)) numbers))
 ;;  (dbgl " x " (get-prop x "item0"))
 ;;  (dbgl " y " (get-prop x "item1"))
 )

