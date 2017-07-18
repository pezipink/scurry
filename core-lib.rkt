#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require threading)
(provide (all-defined-out))

(define-syntax (core-lib stx)
  #'(
     (def-λ (mod-global-list mapper key)
       (def list (get-global key))
       (~ mapper list)
       (set-global key list))

     (def-λ (append-global-list key item)
       (~ mod-global-list (λ (append-list _ item)) key))

     (def-λ (remove-global-list key item)
       (~ mod-global-list (λ (remove-list _ item)) key)) 
     
     (def-λ (mod-prop-list mapper key obj)
       (def list (get-prop obj key))
       (~ mapper list)
       (set-prop obj key list))

     (def-λ (append-prop-list key obj item)
       (~ mod-prop-list (λ (append-list _ item)) key obj))

     (def-λ (remove-prop-list key obj item)
       (~ mod-prop-list (λ (remove-list _ item)) key obj)) 

     (def-λ (fold folder inputs acc)
       (foreach (i inputs)
         (def acc (~ folder acc i)))
       (s-return acc))

     (def-λ (sum inputs)
       (~ fold (λ (a b) (add a b)) inputs 0))
     
     (def-λ (map mapper inputs)
       (def out (s-list))
       (foreach (i inputs)
         (append-list out (~ mapper i)))
       (s-return out))

     (def-λ (filter pred inputs)
       (def out (s-list))
       (foreach (i inputs)
         (s-when (~ pred i)
           (append-list out i)))
       (s-return out))
     
     (def-λ (partition pred inputs)
       (def x (s-list))
       (def y (s-list))
       (foreach (i inputs)
         (s-if (~ pred i)         
           (append-list x i)
           (append-list y i)))
       (tuple x y))

     ))
