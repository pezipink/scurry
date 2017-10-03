#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require threading)
(provide (all-defined-out))

(define-syntax (core-lib stx)
  #'(begin
     (def global (global-obj))
      
     (def-λ (mod-global-list mapper key)
       (def list (get-global key))
       (mapper list)
       (set-global key list))

     (def-λ (append-global-list key item)
       (mod-global-list (λ (append-list _ item)) key))

     (def-λ (remove-global-list key item)
       (mod-global-list (λ (remove-list _ item)) key))
     
     (def-λ (fold inputs acc folder)
       (foreach (i inputs)
         (def acc (folder acc i)))
       (return acc))

     (def-λ (sum inputs)
       (fold (λ (a b) (add a b)) inputs 0))

     (def-λ (each l f)
       (foreach (x l) (f x)))

     (def-λ (each inputs f)
       (foreach (i inputs) (f i)))
     
     (def-λ (map inputs mapper)
       (def out (list))
       (foreach (i inputs)
         (append-list out (mapper i)))
       (return out))

     (def-λ (filter inputs pred)
       (def out (list))
       (foreach (i inputs)
         (when (pred i)
           (append-list out i)))
       (return out))
     
     (def-λ (partition inputs pred)
       (def x (list))
       (def y (list))
       (foreach (i inputs)
         (if (pred i)         
           (append-list x i)
           (append-list y i)))
       (tuple x y))

     (def-λ (split-top n inputs)
       (def i (sub n (list-len inputs)))
       (split-at i #f inputs))

     (def-λ (split-bottom n inputs)       
       (split-at n #t inputs))

     (def-λ (append-many source dest)
       (foreach (i source) (append-list dest i)))

     (def-λ (prepend-many source dest)
       (foreach (i source) (prepend-list dest i)))
     
     (def-λ (deal from-obj from-prop to-obj to-prop n)
       ;todo: visibilty
       (def source (get-prop from-obj from-prop))
       (def dest (get-prop to-obj from-prop))
       (def new (split-top n source))
       (append-many new dest)
       (sync-prop from-obj from-prop)
       (sync-prop to-obj to-prop)
       )

     (def-λ (max x y)
       (if (gt x y)
         (return x)
         (return y)))

     (def-λ (min x y)
       (if (lt x y)
         (return x)
         (return y)))
     
     (def-λ (lists-same? a b)
       (def al (list-len a))
       (def bl (list-len b))
       (if (ne al bl)
           (return #f)
           (begin
             (def i 0)
             (while (and (ne i al)
                         (eq (nth i a) (nth i b)))
               (++ i))
             (return (eq i al)))))

     (def-λ (dbg-obj obj)
       (foreach (k (keys obj))
         (dbgl k ":" (get-prop obj k))))
     
     (def-λ (first pred input)
       (def res #f)
       (def i 0)
       (def len (list-len input))
       (while (and (lt i len) (eq res #f))
         (when (pred (nth i input))
           (def res (nth i input)))
         (++ i))
       (return res))

           
     (def-λ (flow-from-triple clientid title triples)
       (def-flow req title)
       (def-obj result-map)
       (foreach (t triples)
                (set-prop result-map t.item0 t.item2)
                (add-flow-action req t.item0 t.item1))
       (def res (flow req clientid))
       ((get-prop result-map res) res))

     
     ))
