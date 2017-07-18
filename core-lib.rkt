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
       (~ fold
          (λ (acc i)
            (s-begin
             (append-list acc (~ mapper i))
             (s-return acc)))
          inputs
          '(createlist)))

     (def-λ (filter pred inputs)
       (~ fold
          (λ (acc i)
            (s-if (~ pred i)
              (s-begin
               (append-list acc i)
               (s-return acc))
              (s-return acc)))
          inputs
          '(createlist)))

     (def-λ (partition pred inputs)
       (~ fold
          (λ (acc i)
            (s-begin
             (s-if (~ pred i)
              (~ append-prop-list "item0" acc i)
              (~ append-prop-list "item1" acc i))
             (s-return acc)))
          inputs
          (s-begin
           (def-obj output)
           (set-props output (["item0" '(createlist)]
                              ["item1" '(createlist)]))
           (s-return output))))

     
     ))
