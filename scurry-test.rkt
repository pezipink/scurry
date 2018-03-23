#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     racket/format))

;; (define-syntax-parser def-fjet

;;   [(_ name initial-apply ([key value] ...))
;;    #`(begin
;;        (def-obj name
;;          (["app" 0]
;;           ["apply" initial-apply]
;;           [key value] ...))
;;        (set-prop name "app" (λ ((get-prop name "apply") name _))))])


(define-syntax Zzz
 (syntax-rules ()
    ((_ g) (λ (s/c) (λ (_) (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (disj (Zzz g0) (disj+ g ...))]))

(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g0 g ...) (conj+ g0 g ...)]
    [(_ (x0 x ...) g0 g ...)
    (call/fresh (λ (x0) (fresh (x ...) g0 g ...)))]))

(define-syntax run-raw
  (syntax-rules ()
    [(_ n (x ...) g0 g ...)     
     (take n (call/empty-state
              (fresh (x ...) g0 g ...)))]))

(define-syntax run
  (syntax-rules ()
    [(_ n (x ...) g0 g ...)
     (map-l reify-1st
            (take n (call/empty-state
                               (fresh (x ...) g0 g ...))))]))

(define-syntax run2
  (syntax-rules ()
    [(_ n (x ...) g0 g ...)
     (map-l reify-2st
            (take n (call/empty-state
                               (fresh (x ...) g0 g ...))))]))


;; (match test
;;   ; if key 1 exists
;;   [(create-obj (["key1" (cons h _)])) (dbgl "head is " h)]
;;   ;if key 2 exists
;;   [{ "key2" (cons _ t) } (dbgl "taild is " t)])

;;becomea

;; (let
;;     [(result
;;       (run 1 (q)
;;            (fresh [h]
;;             (=== (list h) q)
;;             (=== create-obj (["key1" (cons h _)])))))]
;;   [h (head result)])


(begin-for-syntax
  (define-splicing-syntax-class logic-vars
    [pattern {~seq var ...}
             #:with [n ...] (map ~a (range (length (attribute var))))]))


(define-syntax (logic stx)
  (syntax-parse stx
    [(_ obj (vars:logic-vars) [expr f])
     #'(let ([result
               (run-raw 1 (vars.var ...)                          
                        (=== expr obj))]
              [vars.var (map-l (reify-nth vars.n) result)] ...)
          f)]))
     
             
     
;; (let (
;;       (result (run 1 (q)
;;                    (fresh (h t)
;;                           (=== (create-obj ((h h) (t t))) q)
;;                           (=== (create-obj (("key1" (cons h t)))) test)
;;                           )))
;;       (h (get-prop h result))
;;       (t (get-prop t result)))
;;   (dbgl "tail is " t))
  
;; (dbgl "!! " (run 10 (q)
;;                  (fresh [h t v]
;;                  (=== test (create-obj (["key1" (cons h t)]
;;                                         ["key2" v])))
;;                  (=== q (list h t v))
                        

;;                        )))


(scurry


 
 (import core-lib)
 ;; (def players (vals (get-players)))
 (assert (is-list? (list)))
  (dbgl "HERE")
 (assert (is-list? (list)))
 (dbgl "HERE2")
 ; (assert (not (is-list? 10)))
 ;(assert (not (is-arr? (list 20 20))))
 ;; (def empty (list))

 ;; (def test1 (list 10))
 
 ;; (dbgl "test1 " (head test1))
 
 ;; (def test2 (list 20 40))


 
 ;; (dbgl "test2 " (head (tail test2)))

 ;; (def-λ (test-rec lst)
 ;;   (if (eq lst #f)
 ;;       (dbgl "end of list")
 ;;       (begin
 ;;         (dbgl "item : " (head lst))
 ;;         (test-rec (tail lst)))))

 ;; (test-rec test2)


 (def-λ (var c) (arr c))
 (def-λ (var? x) (is-arr? x))
 (def-λ (var=? x1 x2)
;   (or (eq x1 x2)
       (eq (nth x1 0) (nth x2 0)));)
 ;(def-λ (var=? x1 x2) (eq x1 x2))
 (def-λ (assp f l)
;   (dbgl "assp " f " " l)
   (if (eq l #f)
       #f
       (begin
         (def h (head l))
 ;        (dbgl "h is " h)
         (if (is-list? h)
             (begin
               (def h2 (head h))
;               (dbgl "h2 is " h2 " f " (f h2))
               (if (and h2 (f h2))
                   (begin
                     ;(dbgl "jdksdsjk")
                     (return h))
                   (if (is-list? l)
                       (assp f (tail l))
                       #f)))
             #f))))

 (def-λ (walk u s)
;   (dbgl "walk u " u " s " s)
   (if (not (var? u)) u       
       (let ([pr (assp (λ (var=? u _)) s)])
 ;        (dbgl " PR = " pr)
         (if (and pr (is-list? (tail pr))) (walk (head (tail pr)) s) u))))


 (def-λ (ext-s x v s)   (cons (list x v) s))


 ;yuck - this should be done in the vm
 (def-λ (keys->list h)
   (def k (keys h))
   (def i -1)
   (def l ((len k) - 1))
   (def-λ (aux acc)
     (if (i < l) 
         (begin
           (++ i)
           (aux (cons (nth k i) acc)))
         acc))
   (aux (list)))
     
   
 (def-λ (unify u v s)
;  (dbgl "unify u " u " v " v " s " s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]

      ;map unification
      [(and (is-obj? u) (is-obj? v))
       (begin
;        (dbgl "unify maps " u " " v)
         (let ([unify-maps
                (λ (u2 v2 l2 s2)
                  (begin ;(dbgl "unify maps inner " u2 " " v2 " " l2 " " s2)
                         (if (eq l2 #f) s2
                             (let ([k (head l2)])
                               (if (and (contains u2 k) (contains v2 k))
                                   (begin
                          ;           (dbgl "keys exist in both, unifiying values..")
                                     (def s3 (unify (get-prop u2 k) (get-prop v2 k) s2))
                           ;          (dbgl "unify res " s3)
                                     (unify-maps u2 v2 (tail l2) s3))
                                   (begin
                                     
                                     (dbgl "keys did not exist in both.")

                                     ;maybe the key is a logic variable.
                                     ;if it is we can try and unify it with key from the other
                                     ;dictionary.
                                     ;this is a n2 worst case lookup!  revist this later
                                     (if (var? k)
                                         (begin
                                           (dbgl "attempting to unify key")
                                           (def lkey 0)
                                           (def rkey 0)
                                           (def lkeys (keys u2))
                                           (def rkeys (keys v2))
                                           (def done #f)
                                           (def s3 s2)
         ;                                  (dbgl "s3 is " s3)
                                           (for (x rkeys)
                                             #:break (done = 1)

       ;                                      (dbgl "done is " done)
      ;                                       (dbgl "attept key unification with " x)
                                             (def s4 (unify k x s3))
                                             (when (is-list? s4)
        ;                                       (dbgl "unified key with " x)
                                               ;attempt to unify with value
                                               ;(s3 <- s4)
;                                               (dbgl "s3 is " s3)
                                               (def s5 (unify (get-prop u2 k) (get-prop v2 x) s4))
;                                              (dbgl "s5 is " s5)
                                               (when (is-list? s5)
                                                 (begin
                                                   (s3 <- s5)
  ;                                                 (dbgl "s3 is " s3)
                                                   ;  (dbgl "unified key. ")
                                                   )
                                                 )                                       
                                               )
                                             )
    ;                                       (dbgl "end loop")
     ;                                      (dbgl "s3 is " s3)
                                           (unify-maps u2 v2 (tail l2) s3)
                                           )
                                         
                                     
                                     
                                         (unify-maps u2 v2 (tail l2) s2)
     ;                                    #f
                                         )))))))]                                     
               [key-list (keys->list u)])
;           (dbgl "herhe!!" key-list)
           (unify-maps u v key-list s)))]

      
      
      ;; list unification
      ((and (is-list? u) (is-list? v))
       (begin
;       (dbgl "unify lists " u " " v)
       (let ((s2 (unify (head u) (head v) s))) 
         (begin   ;         (dbgl "unify head result was " s2)
         (if (is-list? s2)
             (unify (tail u) (tail v) s2)
             #f)))))
      (else
       (begin
         ;; (dbgl "eq " u " = " v)
         ;; (dbgl "u is list " (is-list? u))
         ;; (dbgl "v is list " (is-list? v))
         (if (eq u v)
             (begin

               (return s))
             #f))))))
 
 (def-λ (map-l f l)
;   (dbgl "map-l " f " " l)
   (def-λ (aux l acc)
     (if (eq l #f) acc (aux (tail l) (cons (f (head l)) acc))))
   (aux l (list)))

 (def-λ (len-l l)
   (def-λ (aux l acc)
     (if (eq l #f) acc (aux (tail l) (add acc 1))))
   (aux l 0))
 
 (def-λ (reify-name n)
   (add "_." n))
 
 (def-λ (reify-s v s)
;   (dbgl "reify-s " v s)
   (let ([v (walk v s)])
     (cond
       [(var? v)
        (ext-s v (reify-name (len-l s)) s)]

       [(is-list? v)     
        (reify-s (tail v) (reify-s (head v) s))]
       [else s])))

 (def-λ (walk* v s)
;  (dbgl "walk* " v " " s)
   (let ([v (walk v s)])
;     (dbgl "v was " v)
     (cond
       [(var? v) v]
       [(is-list? v)
        (begin
;          (dbgl "!!")
          (cons (walk* (head v) s)
                (walk* (tail v) s)))]
       [else v])))
 
 (def-λ (reify-1st s/c)
;  (dbgl "reify-1st")
   (let ([v (walk* (var 0) (head s/c))])
;     (dbgl "v is " v)
     (walk* v (reify-s v (list)))))

 (def-λ (reify-nth n s/c)
 ; (dbgl "reify nth " n " " s/c)
  (let ([v (walk* (var n) (head s/c))])
;      (dbgl "v is " v )
      (walk* v (reify-s v (list)))))
  
 ;; (def-λ (mK-reify s/c*)
 ;;   (map-l reify-state/1st-var s/c*))
 
(def-λ (units s/c) (cons s/c (list)))

(def-λ (=== u v)
  (λ (s/c)
;    (dbgl "head sc is " (head s/c))
 ;   (dbgl "tail sc is " (tail s/c))
    (let ([s (unify u v (head s/c))])
      (if (is-list? s) (begin
 ;             (dbgl "unifed!!")
              (units (list s (head (tail s/c)))))
          (list)))))



(def empty-state (list (list) 0))
(def-λ (call/empty-state g) (g empty-state))

(def-λ (call/fresh f)
  (λ (s/c)
    (let ((c (head (tail s/c))))
      (begin ;(dbgl "in call/fresh c is  " c " s/c is " s/c)
      ((f (var c)) (list (head s/c) (c + 1)))))))

(def-λ (mplus $1 $2)
;  (dbgl "mplus " $1 " ;; " $2 )
  (cond
    ((eq $1 #f) $2)
    ((is-func? $1)
     (begin
 ;      (dbgl "IS FUNC")
       (λ (x) (mplus $2 ($1 x)))))
    (else (cons (head $1) (mplus (tail $1) $2)))))

(def-λ (bind $ g)
;  (dbgl "BIND " $ " " g)
  (cond
    [(eq $ #f)
     (begin
       ;(dbgl "empty list")
       (list))]
    [(is-func? $) (λ (x) (bind ($ x) g))]
    [else
     (begin
       ;(dbgl "appending..." )
       (mplus (g (head $)) (bind (tail $) g)))]))

(def-λ (disj g1 g2) (λ (s/c) (mplus (g1 s/c) (g2 s/c))))

(def-λ (conj g1 g2) (λ (s/c) (bind (g1 s/c) g2)))


(def-λ (fives x)
  (disj+
   (=== (var "x") 5)
   (fives x)
   ))


(def-λ (sixes x)
  (disj+
   (=== (var "x") 6)
   (sixes x)))

(def fives-and-sixes
  (call/fresh (λ (x) (disj+ (fives x) (sixes x)))))

(def lag (fives-and-sixes empty-state))

(def-λ (pull $)
  (if (is-func? $) (pull ($ 0)) $))

(def-λ (take-all $)
  (let ([$ (pull $)])
      (if (eq $ #f) (list) (cons (head $) (take-all (tail $))))))

(def-λ (take n $)
  (if (n = 0) (list) 
      (let ([$ (pull $)])
        (cond
          [(eq $ #f) (list)]
          [else (cons (head $) (take (n - 1) (tail $)))]))))

(def u# (=== #t #f))
(def s# (=== #t #t))

;(dbgl (take 20 (lag)))
(dbgl "conj")
;(dbgl (unify (list 1 2 3) (list 1 2 (var 0)) empty-state))


;; (dbgl
;;  (run 13 (q) fives-and-sixes))
;; (dbgl
;;  (run 3 (q)
;;       (fresh (v)
;;              (=== (cons v 2 ) (cons 1 q))
;;    )))



(def test {10 20 "h" 42})
(def-λ (caro p a)
  (fresh [d]
    (=== (cons a d) p)))

(def-λ (cdro p a)
  (fresh [d]
    (=== (cons d a) p)))

(def-λ (conso x y z)
  (=== (cons x y) z))

(def-λ (propo o k v)
  (let ([v (contains o k)])
    (conde
     [(=== v #t) (=== v (get-prop o k))]
     [u# u#])))



;; (dbgl "go"
;;       (run 1 (q)
;;            (fresh [v]
;;                   (=== q (create-obj))
;;                   (=== v (get-prop q "k")))))

(def-λ (nullo x)
  (=== x #f))

(def-λ (eq-caro l x)
  (caro l x))

(def-λ (rembero x l out)
  (conde
   [(nullo l) (=== #f out)]
   [(eq-caro l x) (cdro l out)]
   [s#
    (fresh [res]
           (fresh [d]
                  (cdro l d)
                  (rembero x d res))
           (fresh [a]
                  (caro l a)
                  (conso a res out)))]))
    

(def-λ (membero x l)
;  (dbgl "membero " x " " l)
  (conde
;   [(=== #f l) u#]
   [(caro l x) (cdro l #f)]
   [(caro l x) s#]
   [
    (fresh [d]
     (cdro l d)
     (membero x d))]))

(def-λ (on-righto e1 e2 l)
;  (dbgl "on-righto " e1 " " e2 " " l)
  (fresh [h t]
    (conde
     [(nullo l) u#]
     [(caro l e1) (cdro l t) (caro t e2)]
     [(cdro l t) (on-righto e1 e2 t)])))

(def-λ (nexto e1 e2 l)
;  (dbgl "nexto " e1 " " e2 " " l)
  (conde
   [(on-righto e1 e2 l)]
   [(on-righto e2 e1 l)]))
     
;; (dbgl "membero")
;; (dbgl
;;  (run 1 (q)
;;       (=== q 10)
;;       (membero q (list 1 2 3 4 5 6))))

(def-λ (list->arr l)
  (def out (list))
  (def-λ (aux l)
    (when (eq l #f) out)
    (append-arr out (head l))
    (aux (tail l)))
  (aux l))
                 
      

(def-λ (appendo l r out)
  (conde
   [(=== l #f) (=== r out)]
   [(fresh [a d res]
          (conso a d l)
          (conso a res out)
          (appendo d r res))]))


;; (dbgl "TRS")
;; (dbgl "1")
;; (run 1 (q)
;;      u#)

;; (dbgl "10:"
;;   (run 1 (q)
;;     u#))

;; (dbgl "11"
;;   (run 1 (q)
;;     (=== #t q)))

;; (dbgl "12"
;;       (run 1 (q)
;;            u#
;;            (=== #t q)))
;; (dbgl "13"
;;       (run 1 (q)
;;            s#
;;            (=== #t q)
;;            ))

;; (dbgl "18"
;;       (run 1 (q)
;;            s#
;;            (=== #f q)))

;; (dbgl "26"
;;       (run 1 (q)
;;            (fresh [x]
;;                   (=== x #t)
;;                   (=== #t q))))

;; (dbgl "28"
;;       (run 1 (q)
;;            s#))


;; (dbgl "29"
;;       (run 1 (x)
;;            (let ([x #f])                    
;;              (fresh [x]
;;                     (=== #t x)))))


;; (dbgl "30"
;;       (run 1 (r)
;;            (fresh [x y]
;;                   (=== (cons x (cons y #f)) r))))
;; (dbgl "31"
;;       (run 1 (s)
;;            (fresh [t u]
;;                   (=== (cons t (cons u #f)) s))))

;; (dbgl "32"
;;       (run 1 (r)
;;            (fresh [x]
;;                   (let ([y x])
;;                     (fresh [x]
;;                            (=== (cons y (cons x (cons y #f))) r))))))

;; (dbgl "34"
;;       (run 1 (q)
;;            (=== #f q)
;;            (=== #t q)))

;; (dbgl "35"
;;       (run 1 (q)
;;            (=== #f q)
;;            (=== #f q)))

;; (dbgl "36"
;;       (run 1 (q)
;;            (let ([x q])
;;              (=== #t x))))

;; (dbgl "37"
;;       (run 1 (r)
;;            (fresh [x]
;;                   (=== x r))))

;; (dbgl "44"
;;       (run 1 (q)
;;            (conde
;;             [u# s#]
;;             [u# u#])))
;; (dbgl "47"
;;       (run 2 (x)
;;            (conde
;;             [(=== "olive" x) s#]
;;             [(=== "oil" x) s#]
;;             [u# u#])))
             
             
;; (dbgl "50"
;;       (run 3 (x)
;;            (conde
;;             [(=== "virgin" x) u#]
;;             [(=== "olive" x) s#]
;;             [s# s#]
;;             [(=== "oil" x) s#]
;;             [u# u#])))
             


             
;; (dbgl "55"
;;       (run 2 (r)
;;            (fresh [x y]
;;            (conde
;;             [(=== "" x) u#]
;;             [(=== "split" x) (=== "pea" y)]
;;             [(=== "navy" x) (=== "bean" y)]
;;             [u# u#])
;;            (=== (list x y "soup") r))))
             
;; (dbgl "57"
;;       (let ([teacupo (λ (x)
;;                        (conde
;;                         [(=== "tea" x) s#]
;;                         [(=== "cup" x) s#]
;;                         [u# u#]))])
;;         (run 5 (r)
;;              (fresh [x y]
;;                     (conde
;;                      [(teacupo x) (=== #t y) s#]
;;                      [(=== #f x) (=== #t y)]
;;                      [u# u#])
;;                     (=== (cons x (cons y #f)) r)))))


;; (dbgl "2.7"
;;   (run 2 (q)
;;     (caro (cons 1 (cons 2 #f)) 1)
;;     (=== #t q)))

;; (dbgl "2.15"
;;       (run 1 (r)
;;            (fresh [v]
;;                   (cdro (list 1 2 3) v)
;;                   (caro v r))))
;; (dbgl "2.20"
;;       (run 1 (x)
;;            (cdro (list 1 2 3 4) (list x 3 4))))

;; (dbgl "2.21"
;;       (run 1 (l)
;;            (fresh [x]
;;                   (cdro l (list "c" "o" "r" "n"))
;;                   (caro l x)
;;                   (=== "a" x))))

;; (dbgl "2.22"
;;       (run 1 (l)
;;            (conso (list 1 2 3) (list 4 5) l)))

;; (dbgl "2.29"
;;       (run 1 (l)
;;            (fresh [d x y w s]
;;                   (conso w (list "a" "n" "s") s)
;;                   (cdro l s)
;;                   (caro l x)
;;                   (=== "b" x)
;;                   (cdro l d)
;;                   (caro d y)                  (=== "e" y))))
           
;; (dbgl "! "  (run 5 (l)
;;               (membero "juan" l)))



;; (dbgl "test "
;;   (run 1 (h)
;;         (fresh (a1 a2 a3 a4 a5
;;                 b1 b2 b3 b4 b5
;;                 c1 c2 c3 c4 c5
;;                 d1 d2 d3 d4 d5
;;                 e1 e2 e3 e4 e5)
;;            (=== h (list
;;                    (list a1 a2 a3 a4 a5)
;;                    (list b1 b2 b3 b4 b5)
;;                    (list c1 c2 c3 c4 c5)
;;                    (list d1 d2 d3 d4 d5)
;;                    (list e1 e2 e3 e4 e5)))

;;            (fresh (t1 t2 t3)
;;                   (membero (list "englishman" t1 t2 t3 "red") h))
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;                   (on-righto (list t1 t2 t3 t4 "ivory")
;;                              (list t5 t6 t7 t8 "green") h))
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;              (nexto (list "norwegian" t1 t2 t3 t4)
;;                     (list t5 t6 t7 t8 "blue") h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 "kools" t2 t3 "yellow") h))
;;            (fresh (t1 t2 t3)
;;              (membero (list "spaniard" t1 t2 "dog" t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 t2 "coffee" t3 "green") h))
;;            (fresh (t1 t2 t3)
;;              (membero (list "ukrainian" t1 "tea" t2 t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 "luckystrikes" "oj" t2 t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list "japanese" "parliaments" t1 t2 t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 "oldgolds" t2 "snails" t3) h))
;;            (fresh (t1 t2 t3 t4)
;;              (membero (list t1 t2 "water" t3 t4) h))
;;            (fresh (t1 t2 t3 t4)
;;              (membero (list t1 t2 t3 "zebra" t4) h))

;;            (=== a1 "norwegian")
;;            (=== c3 "milk")
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;                   (nexto (list t1 t2 t3 "horse" t4) 
;;                          (list t5 "kools" t6 t7 t8) h))
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;                   (nexto (list t1 t2 t3 "fox" t4)
;;                          (list t5 "chesterfields" t6 t7 t8) h))

;;          )))

;; (dbgl "zebrao "
;;    (run 1 (h)
;;         (fresh (a1 a2 a3 a4 a5
;;                 b1 b2 b3 b4 b5
;;                 c1 c2 c3 c4 c5
;;                 d1 d2 d3 d4 d5
;;                 e1 e2 e3 e4 e5)
;;            (fresh (t1 t2 t3)
;;              (membero (list "englishman" t1 t2 t3 "red") h))
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;              (on-righto (list t1 t2 t3 t4 "ivory")
;;                         (list t5 t6 t7 t8 "green") h))
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;              (nexto (list "norwegian" t1 t2 t3 t4)
;;                     (list t5 t6 t7 t8 "blue") h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 "kools" t2 t3 "yellow") h))
;;            (fresh (t1 t2 t3)
;;              (membero (list "spaniard" t1 t2 "dog" t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 t2 "coffee" t3 "green") h))
;;            (fresh (t1 t2 t3)
;;              (membero (list "ukrainian" t1 "tea" t2 t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 "luckystrikes" "oj" t2 t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list "japanese" "parliaments" t1 t2 t3) h))
;;            (fresh (t1 t2 t3)
;;              (membero (list t1 "oldgolds" t2 "snails" t3) h))
;;            (fresh (t1 t2 t3 t4)
;;              (membero (list t1 t2 "water" t3 t4) h))
;;            (fresh (t1 t2 t3 t4)
;;              (membero (list t1 t2 t3 "zebra" t4) h))
;;            (=== h (list
;;                    (list a1 a2 a3 a4 a5)
;;                    (list b1 b2 b3 b4 b5)
;;                    (list c1 c2 c3 c4 c5)
;;                    (list d1 d2 d3 d4 d5)
;;                    (list e1 e2 e3 e4 e5)))
;;            (=== a1 "norwegian")
;;            (=== c3 "milk")
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;              (nexto (list t1 t2 t3 "horse" t4) 
;;                     (list t5 "kools" t6 t7 t8) h))
;;            (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
;;                   (nexto (list t1 t2 t3 "fox" t4)
;;                          (list t5 "chesterfields" t6 t7 t8) h))
        
;;    )))


;; (def x {"jet" 20 "jk" 39504 })

;; (def test
;;   { "key1" (list 42 58 42 { "key1" 128 }) 
;;     "key2" (λ (add _ _ )) } )

;; (logic test (a b c)
;;   [{"key1" (list a b a {"key1" c})}
;;    (dbgl "results " a " " b " " c)]
;;    ;more cases...
;;        )





;(def l (list (list 1 2 3) (list 4 5 6) 7 8))

(def state
  { "current-player" "medic"
    "players"
    { "medic"
      { "actions-left" 4
        "cards" (list {"name" "london"
                       "colour" "blue"
                       "type" "city"}
                      {"name" "paris"
                       "colour" "blue"
                       "type" "city"}
                      {"name" "mumbai"
                       "colour" "black"
                       "type" "city" } )
        "location" "london"   }
      
      "scientist"
        { "actions-left" 4
          "cards" (list {"name" "new-york"
                         "colour" "blue"
                         "type" "city"}
                        {"name" "shanghai"
                         "colour" "red"
                         "type" "city"}
                        {"name" "cairo"
                         "colour" "black"}
                    )
          "location" "lima" }
       }
    }
  )
    



;; (def results (run 12 (q)
;;   (fresh [n c x b blues]
;;     (=== { "current-player" n
;;            "players" { n { "actions-left" 4
;;                            "cards" c} } } state))))


;; (dbgl (run 12 (q)
;;            (=== { q "medic" } state )))
                         

(def-λ (iter l f)
  (def-λ (aux l)
    (unless (or (not (is-list? l)) (eq l #f))
      (f (head l))
      (aux (tail l))))
  (aux l))

(def-λ (filtero m l out)

  (conde
   [(=== l #f) s#]
   [(fresh [h t res]
     (conso h t lv)
     (conde
      [(=== m h)
       (conso h res out)
       (filtero m t res)]
      [(fresh [h t]
       (conso h t l)
       (filtero m t out))]))]

   ))


(def results (run 1 (q)
  (fresh [n c blues blacks red]
    (=== { "current-player" n
           "players" { "scientist" { "actions-left" 4
                           "cards" c} } } state)
    (filtero {"colour" "black"} c blacks)
    (filtero {"colour" "blue"} c blues)
    (filtero {"colour" "ref"} c red)
    (=== (list blues blacks red) q)
    )))

;; (def results (run 10 (q)
;;                   (fresh [a]
;;                   (filtero 4 (list 1 2 3 4) q)
;;                   )))





;;     ;; (caro c q)
;;     ;; (=== {"colour" "blue"} q)
    
;;     )))

(def-λ (dl x)
  (cond
    [(is-list? x)
     (iter x dl)]
    [(is-obj? x) (dbg-obj x)]
    [else (dbgl x)]))

(dbgl "pritnf")
(dl results)
;(dbg-obj (head(head results)))
;(dbg-obj (head results));(iter (head results) (λ (dbg-obj _)))
                




;; (dbgl "!! " (run 10 (q)
;;                  (fresh [h t v]
;;                  (=== test (create-obj (["key1" (cons h t)]
;;                                         ["key2" v])))
;;                  (=== q (list h t v))
                        
;; (def cat
;;       (run-raw 1 (h t)

                 
;;                   (=== (create-obj (("key1" (cons h t)))) test)
;;                   ))

;; (dbgl cat)

;; (dbgl "!!!! " (run 5 (q)
;;                  (fresh [k v]
;;                         (=== q k)
;;                         (=== (create-obj ([k v])) test))))

;; (dbgl "hello")
;; (run 1 (q)
;;      (fresh [a b]
;;             (=== (cons a b) q)
;;             (=== #t #f)))
            



;; (dbgl "res")
;; (dbgl
;;  (run 1 (q)
;;   (fresh (a b)
;;    (=== q (cons a b))
;;   (appendo a (list 4 b 6) (list 1 2 3 4 5 6)))))


;q = (list 1 2 3 4 5 6)






;; (dbgl "res")
;; (def res
;;   (run 1 (q)
;;        (fresh (a b)
;;               (=== q (cons a b))
;;       (appendo a (list 4 5 b) (list 1 2 3 4 5 6)))))

;; (dbgl  (head res))
; q = ?


;; (def aa (pull lag))
;; (dbgl "aa" aa)
;; (def bb (pull (tail aa)))

;; (def cc (pull (tail bb)))
;; (dbgl "cc" cc)
;; (def dd (pull (tail cc)))
;; (dbgl "dd" dd)
;; (dbgl " y : "
;;       (walk (var "x")
;;             (ext-s (var "x") (var "y")
;;                    (list (list (var "z") (var "x"))
;;                          (list (var "y") (var "z"))))))
 
 ;; (def-obj responses)
 ;; (def-λ (get-choice id)
 ;;   (def a (flow id "make a choice" (["hello" "hello"]["goodbye" "goodbye"])))
 ;;   (def b (flow id "make another choice" (["andrea" "andrea"]["ross" "ross"])))
 ;;   (set-prop responses id (a + b))
 ;;   (flow-end))
 

 ;; (for (p players)  (fork get-choice p.clientid))

 ;; (dbgl "waiting on players")
 
 ;; (join)
 
 ;; (for (p players)
 ;;   (dbgl "player " p.clientid " chose " (get-prop responses p.clientid)))
 
 ;; (dbgl "continuing")

 
 
;; ; (def data (list (list "d" "dec" 683 )(list "qn" "==" 0)))
;;  ;; (match-x (list "d" "dec" 683)
;;  ;;          [(list 10 20) #t])
          
 
;;  ;; (match-x (list "d" "dec" 683)

;; ;;           [(list a b 10) (dbgl a b c)])
;; ;; -

;;  'brk
 ; list tests
 ; split-top removes n elements and creates
 ; a new list from them
;;  (def x (arr 1 2 3 4 5 6 7 8 ))

;;  (def y (split-top 3 x))

;;  ;pop should return a single value not a list
;;  (assert (is-int? (pop-arr x)))

;; ;;  'brk

;;  (assert (arrs-same? (arr 1 2 3 4) x)
;;          "expected [1 2 3 4] but got " x)

;;  (assert (arrs-same? (arr 6 7 8) y)
;;          "expected [6 7 8] but got " y)

;;  ; split-bottom removes n elements from the
;;  ; front, creating a new list from them
;;  (def z (split-bottom 2 x))
;;  (assert (arrs-same? (arr 3 4) x)
;;          "expected [3 4] but got " x)

;;  (assert (arrs-same? (arr 1 2) z)
;;          "expected [1 2] but got " z)
 
;;  ; remove-list removes a single element
;;  ;; (remove-list x 4)
;;  ;; (assert (lists-same? (list 3) x)
;;  ;;         "expected [3] but got " x)

;;  ;; (global.hello <- "world")
;;  ;; (dbgl "hello global " global.hello)
 
 ;; (def-obj function-table
 ;;   (["square" (λ (add _ _))]
 ;;    ["cube"   (λ (add _ _ _))]
 ;;    ["twice"  (λ (f x) (f (f x)))]))

 ;; (dbgl "10 squared :" (function-table.square 10))
 ;; (dbgl "10 cubed :" (function-table.cube 10))
 ;; (dbgl "10 squared squared :"
 ;;       (function-table.twice function-table.square 10))
 ;; (dbg-obj function-table)
 
 ;; (def-obj override-functions
 ;;   (["square"
 ;;     (λ (begin
 ;;          (dbgl "inside debug square with " _)
 ;;          (add _ _)))]))
  
 ;; (def-λ (virtual-dispatch object default-object function-name arg)
 ;;   (if (contains object function-name)
 ;;       ;call speciailised method
 ;;       ((get-prop object function-name) arg)
 ;;       ;call default
 ;;       ((get-prop default-object function-name) arg)))   

 ;; (dbgl "dispatch:"
 ;;       (virtual-dispatch
 ;;        override-functions
 ;;        function-table
 ;;        "square" 10))

;;  ;how to handle things that modify or override some function
;;  ;for example, it usually takes

;;  (def mut 10)
;;  (set-var mut 20)
;;  (assert (mut = 20))

;;  (def-λ (mutation-test)
;;    ; mutating outside of closure scope
;;    (mut <- 30)
;;    (assert (mut = 30))
;;    ; shadow
;;    (def mut 50)
;;    (assert (mut = 50)))

;;  (mutation-test)
;;  (assert (mut = 30))

;;  (def-λ (mutation-test byval)
;;    (assert (mut = 30))
;;    (byval <- 40)
;;    (assert (mut = 30))
;;    (assert (byval = 40)))
 
;;  (mutation-test mut)

 
;;  (def obj2 (clone-obj function-table))
;;  (obj2.test <- "hello")
;;  (assert (obj2.test = "hello"))
;;  (dbgl "ft:")
;;  (dbg-obj function-table) 
;;  (dbgl "obj2")
;;  (dbg-obj obj2) 

;;  ;elvis tests
;;  (def-obj elvis
;;    (["hamburger" (create-obj (["more-hamburgers" 42]))]))

;;  (assert
;;   (if (contains elvis "hamburger")
;;       (let ([burger elvis.hamburger])
;;         (if (contains burger "more-hamburgers")
;;             (eq burger.more-hamburgers 42)
;;             #f))
;;       #f))

;;  (assert
;;    (and (contains elvis "hamburger")
;;         (contains (get-prop elvis "hamburger") "more-hamburgers")
;;         (eq (get-prop (get-prop elvis "hamburger") "more-hamburgers")
;;                       42)))

;;  (assert (not (contains elvis "salads")))
;;  (assert (not elvis?.salads))
;;  (assert (contains elvis "hamburger"))
;;  (assert elvis?.hamburger)
;;  (assert elvis?.hamburger?.more-hamburgers)
;;  (assert (not elvis?.hamburger?.salads))
;;  (assert (not elvis?.salads?.moar-salads))
;;  (assert (elvis?.hamburger?.more-hamburgers = 42))
;;  (assert (elvis.hamburger.more-hamburgers = 42))

;; ;;  (def players (vals (get-players))) 
 
;; ;;  (def-λ (rock-paper-scissors)
;; ;;    (let
;; ;;      ([p1 (nth players 0)]
;; ;;       [p2 (nth players 1)]
;; ;;       [f (λ (clientid)
;; ;;            (flow clientid "Choose a move!"
;; ;;                  (["rock" "Play the rock!"]
;; ;;                   ["paper" "Play the paper!"]
;; ;;                   ["scissors" "Play the scissors!"])))]
;; ;;       [r1 (f p1.clientid)]
;; ;;       [r2 (f p2.clientid)])
;; ;;      (cond
;; ;;        [(r1 = r2) "you tie!"]
;; ;;        [(or
;; ;;          (and (r1 = "rock")     (r2 = "scissors"))
;; ;;          (and (r1 = "scissors") (r2 = "paper"))
;; ;;          (and (r1 = "paper")    (r2 = "rock")))
;; ;;          "player 1 wins!"]
;; ;;        [else "player 2 wins!"])))
        
;; ; test closures and let scoping
;;  (def-λ (test-closure n)
;;    (let ([n2 7]
;;          [f (λ (x y)
;;               (begin
;;                 (def ret (add n n2 x y))
;;                 ; mutation inside closure
;;                 (n2 += 1)
;;                 (return ret)))])
;;      (return f)))

;;  (def test-closure-f (test-closure 37))
;;  (assert (eq (test-closure-f 10 20) 74))
;;  (assert (eq (test-closure-f 10 20) 75))

;;  (def test-list (arr (create-obj (["A" 1]["B" (create-obj (["A" 1]["B" 2]))]))
;;                  (create-obj (["A" 2]["B" 4]))))

;; ;test break and indexed for loops
;; (for (item index test-list)
;;    #:break (index = 1)
;;    (dbg "item["index"]=")
;;    (dbg-obj item)
;;    (dbgl "")) 

;; ;property nesting accessors and operators
;; (def-obj props-obj
;;    (["n" 10]
;;     ["prop1" (create-obj
;;              (["prop2" 42]))]))

;; (props-obj.n += 10)
;; (assert (props-obj.n = 20))
;; (assert (props-obj.prop1.prop2 = 42))
;; (props-obj.prop1.prop2 <- 58)
;; (assert (props-obj.prop1.prop2 = 58))
;; (props-obj.prop1.prop2 ++)
;; (assert (props-obj.prop1.prop2 = 59))
;; (props-obj.prop1.prop4 <- (create-obj (["final" (create-obj (["n" 10]))])))
;; (assert (props-obj.prop1.prop4.final.n = 10))
;; (props-obj.prop1.prop4.final.n --)
;; (assert (props-obj.prop1.prop4.final.n = 9))
















;; ;; ; runtime type tests


;; (assert (is-obj? props-obj))
;; (assert (is-arr? (arr)))



;; ;U and Y combinators
;; (def-λ (U f) (f f))


;; ((U (λ (self)
;;       (λ (num)
;;         (if (num = 0)
;;             (dbgl "finished!")
;;             (begin
;;               (dbgl "counting down " num)
;;               ((self self) (sub num 1)))))))
;;  10)

;; (def-λ (mk-counter f num)
;;   (if (num = 0)
;;       (dbgl "finished!")
;;       (begin
;;         (dbgl "counting down " num)
;;         (f (sub num 1)))))
                  
;; (def-λ (Y f)
;;   ((λ (g) (g g))
;;    (λ (g)
;;      (f (λ (x) ((g g) x))))))

;; ((Y mk-counter) 10)

;; ; funcjets
;; ;; (def-λ (create-restorable-function init)
;; ;;   (create-obj
;; ;;    (["prev" (list)]
;; ;;     ["app" init])))


;; (def-obj restorable-func
;;   (["prev" (list)]
;;    ["y" 10]
;;    ["apply" (λ (self x y z) (add self.y x))]
;;    ["app" 0]))

;; (restorable-func.app <- (λ (x) (restorable-func.apply restorable-func x)))


;; (def-λ (push-func obj new-func)
;;   (append-list obj.prev obj.app)
;;   (obj.app <- new-func))

;; (def-λ (pop-func obj)
;;   (obj.app <- (pop-list obj.prev)))


;; 'brk
;; (dbgl (restorable-func 10))

;; (push-func restorable-func (λ (x) (add x x x)))

;; 'brk
;; (dbgl (restorable-func 10))

;; (pop-func restorable-func)





;; ;; (def-fjet test
;; ;;   (λ (this state) (this.default-action this state))
;; ;;   (["default-action"
;; ;;     (create-restorable-function
;; ;;      (λ (this state) (map this.available-actions (λ (~ _ state)))))]
;; ;;    ["available-actions"
;; ;;     (list (create-restorable-function (λ (eq _ 10)))
;; ;;           (create-restorable-function (λ (eq _ 210))))]))

'brk
;(dbgl (test 210))

;; 'brk
;; 'brk


    ;; (dbgl "end")

    )


