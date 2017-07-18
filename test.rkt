#lang racket
(require "asm.rkt")
(require threading)
(require "core-lib.rkt")

(scurry
 (import core-lib)

 ;create list of events
 (set-global "on-juan" '(createlist))

 ; function to raise events
 (def-λ (raise-event event-handlers event-arg)
   (foreach (h event-handlers)
      (~ h event-arg)))

 ;add some events
 (~ append-global-list "on-juan" (λ (dbgl _)))
 (~ append-global-list "on-juan" (λ (dbgl (add _ 42))))
 
 (def numbers (s-list 1 2 3 4 5))
 (def some-number 42)

 (def (x y) (tuple 10 20))


 (def t (tuple 10 20))

 (def (a b) t)
 
 (def-λ (tuple-test ((x y)))
   (dbgl " X " x " Y " y))
 

 (dbgl "1TUPLE " x " " y)

 (~ tuple-test t)
 
 (foreach (n numbers)
   (~ raise-event (get-global "on-juan") n))
  
 (~>> 
  ;pipeline
  numbers
  (~ filter (λ (gt _ 3)))
  (~ map    (λ (add _ some-number)))
  (dbgl))

  (def x (~ partition (λ (gt _ 3)) numbers))
  (dbgl " x " (get-prop x "item0"))
  (dbgl " y " (get-prop x "item1"))
 )

(require (for-syntax syntax/parse))
