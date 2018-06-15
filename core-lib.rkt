#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require threading)
(provide (all-defined-out))

(define-syntax (core-lib stx)
  #'(begin
     (def global (global-obj))
     
     (def-λ (mod-global-arr mapper key)
       (def arr (get-global key))
       (mapper arr)
       (set-global key arr))

     (def-λ (append-global-arr key item)
       (mod-global-arr (λ (append-arr _ item)) key))

     (def-λ (remove-global-arr key item)
       (mod-global-arr (λ (remove-arr _ item)) key))
     
     (def-λ (fold inputs acc folder)
       (for (i inputs)
         (acc <- (folder acc i)))
       (return acc))

     (def-λ (sum inputs)
       (fold inputs 0 (λ (a b) (add a b))))

     (def-λ (each inputs f)
       (for (i inputs) (f i)))
     
     (def-λ (map inputs mapper)
       (def out (arr))
       (for (i inputs)
         (append-arr out (mapper i)))
       (return out))

     (def-λ (id f) (return f))

     (def-λ (clone-arr arr)
       (map arr id))

     (def-λ (clone-append-arr arr v)
       (def out (clone-arr arr))
       (append-arr out v)
       (return out))
       
     (def-λ (filter inputs pred)
       (def out (arr))
       (for (i inputs)
         (when (pred i)
           (append-arr out i)))
       (return out))

     
     (def-λ (first inputs pred)
       (def out 0)
       (def found #f)
       (for (i inputs) #:break (found = #t)
         (when (pred i)
           (begin
             (found <- #t)
             (out <- i))))
       (return out))

     (def-λ (filter-map inputs pred mapper)
       (def out (arr))
       (for (i inputs)
         (when (pred i)
           (append-arr out (mapper i))))
       (return out))
     
     (def-λ (partition inputs pred)
       (def x (arr))
       (def y (arr))
       (for (i inputs)
         (if (pred i)         
           (append-arr x i)
           (append-arr y i)))
       (tuple x y))

     (def-λ (split-top n inputs)
       (def i (sub (len inputs) n))
       (split-at i #f inputs))

     (def-λ (split-bottom n inputs)       
       (split-at n #t inputs))

     (def-λ (append-single arr new-value)
       (append-arr arr new-value)
       (return arr))

     (def-λ (append-many dest source)
       (for (i source) (append-arr dest i))
       (return dest))

     (def-λ (prepend-many  dest source)
       (for (i source) (prepend-arr dest i)))
     
     (def-λ (deal from-obj from-prop to-obj to-prop n)
       ;todo: visibilty
       (def source (get-prop from-obj from-prop))
       (def dest (get-prop to-obj from-prop))
       (def new (split-top n source))
       (append-many new dest)
       (sync-prop from-obj from-prop)
       (sync-prop to-obj to-prop)
       )
     
     ;; (def-λ (digit->string c)
     ;;   (case c
     ;;     [0 (return "0")]
     ;;     [1 (return "1")]
     ;;     [2 (return "2")]
     ;;     [3 (return "3")]
     ;;     [4 (return "4")]
     ;;     [5 (return "5")]
     ;;     [6 (return "6")]
     ;;     [7 (return "7")]
     ;;     [8 (return "8")]
     ;;     [9 (return "9")]))

     ;; (def-λ (int->string num)
     ;;   (def base 10)
     ;;   (def res "")
     ;;   (while (num > 0)
     ;;     (def x (mod num base))
     ;;     (when (base > 10)
     ;;       (x <- (div x (int 10 base))))
     ;;     (res <- (add (digit->string x) res))
                        
               
     ;;     (num <- (sub x num))
     ;;     (base <- (mul base 10)))
     ;;   (return res))
             
               
       
     ;; (def-λ (char->int c)
     ;;   (case c
     ;;     [48 (return 0)]
     ;;     [49 (return 1)]
     ;;     [50 (return 2)]
     ;;     [51 (return 3)]
     ;;     [52 (return 4)]
     ;;     [53 (return 5)]
     ;;     [54 (return 6)]
     ;;     [55 (return 7)]
     ;;     [56 (return 8)]
     ;;     [57 (return 9)]))

     ;; (def-λ (pow x y)
     ;;   (cond 
     ;;     [(eq y 0) (return 1)]
     ;;     [(eq y 1) (return x)]
     ;;     [else
     ;;      (begin
     ;;        (def tot x)
     ;;        (while (y > 1)
     ;;          (tot <- (mul x tot))
     ;;          (y -= 1))
     ;;        (return tot))]))

     ;; (def-λ (str->int str)
     ;;   (def tot 0)
     ;;   (for (c str)
     ;;     (tot <- (add (mul 10 tot) (char->int c))))
     ;;   (return tot))
       
     (def-λ (abs x)
       (if (lt x 0)
           (begin
             (eval-arg x)
             'dup
             'swap
             'pop
             'neg                         
             )
           x))
     
     (def-λ (max x y)
       (if (gt x y)
         (return x)
         (return y)))

     (def-λ (min x y)
       (if (lt x y)
         (return x)
         (return y)))

     (def-λ (max-arr inputs)
       (fold inputs
             (nth inputs 0)
             (λ (acc n)
               (max acc n))))

     (def-λ (binary-arr->int input)
       (def i ((len input) - 1))
       (def res 0)
       (def p 0)
       (while (i > -1)
         (def current (nth input i))
         (when (current = 1)
           (res += (pow 2 p)))
         (i --)
         (p ++))
       (return res))
           
       
     (def-λ (int->binary-arr n)
       (if (n = 0)
           (arr 0 0 0 0 0 0 0 0)
           (begin
             (def res (arr))

             (while (n > 1)
               (def rem (n % 2))
               (prepend-arr res rem)
               (n <- (n / 2)))

             (when (n > 0)
               (prepend-arr res 1))

             (while (let ([l (len res)]
                          [r (l % 8)])
                      (r != 0))                    
               (prepend-arr res 0))
             (return res))))

     
     (def-λ (int->hex n)
       (def-λ (->hex n)
         (case n
           [10 (return "A")]
           [11 (return "B")]
           [12 (return "C")]
           [13 (return "D")]
           [14 (return "E")]
           [15 (return "F")]
           [else (->str n)]))

       (def res "")       
       (while (n > 15)
         (def rem (n % 16))
         (res <- (add (->hex rem) res))
         (n <- (n / 16)))
       
       (when (n > 0)
         (res <- (add (->hex n) res)))
         
       (return res))
     
       
     (def-λ (copy-val n)
       (eval-arg n)
       'dup
       'swap
       'pop)
     
     (def-λ (arrs-same? a b)
       (def al (len a))
       (def bl (len b))
       (if (ne al bl)
           (return #f)
           (begin
             (def i 0)
             (while (and (ne i al)
                         (eq (nth a i) (nth b i)))
               (++ i))
             (return (eq i al)))))

     (def-λ (dbg-obj obj)
       (for (k (keys obj))
         (dbgl k ":" (get-prop obj k))))

     ;flow from triple
     (def-λ (fft triples clientid title)
       (def-flow req title)       
       (def-obj result-map)       
       (for (t triples)
         (set-prop result-map t.item0 t.item2)
         (add-flow-action req t.item0 t.item1))
       (def res (flow req clientid))
       ((get-prop result-map res) res))

     (def-λ (map-flow items client title tripler)
       (~> (map items tripler)  (fft client title)))


     ))
