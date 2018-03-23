;#lang s-exp "asm.rkt"
#lang racket
(require syntax/parse/define)
(require (for-syntax syntax/parse
                     racket/string))
(require (for-syntax racket/set))
(require (for-syntax racket/format))
(require (for-syntax syntax/srcloc))
;(require racket/match)
(require (for-syntax racket/list))
(require racket/list)
(require (for-syntax racket/match))
(require racket/trace)
(require racket/string)
(require (for-syntax racket/syntax))
(require threading)


(define-syntax-rule (wdb msg args ...)
  (writeln (format msg args ...)))


(define input '(p Z (h Z W)  (f W)))
(define input2 '(p (f X) (h Y (f a)) Y))

(define input3 '(f X (g X a)))
(define input4 '(f b Y))



(pair? '(1))
;process an item
; if it has not been seen before, assign it a register
; otherwise, continue
; map across the finished registers and replace items in
; a list with their register

(struct node (item depth))
(define (functor->register f)  
  (define (aux temp i d items)
    (cond
      [(pair? items)
       (let*-values
           ([(item) (car items)]
            [(temp i) (if (hash-has-key? temp item)
                          (values temp i)
                          (values (hash-set temp item (cons (format "X~a" i) d)) (add1 i)))])
         (if (pair? item)
             (aux temp i (add1 d) (append (cdr items) (cdr item)))
             (aux temp i d (cdr items))))]
      [else temp]))
  (define mapping (aux (make-immutable-hash) 1 1(list f)))
  (define (transform l acc)
    (cond
      [(pair? l)
       (let ([item (car l)])
         (transform (cdr l) (cons (hash-ref mapping item item) acc)))]
      [(not (eq? l '())) (cons l acc)]
      [else  (reverse acc)]))
  ;transfom the mappigs
  ;; (for/hash ([(k v) mapping])
  ;;   (wdb "~a ~a" k v)
  ;;   (values v (transform k '())))
  mapping
  )
  
;; (define (compile-query register-map f)  
;;   (define regs (functor->register f))

;;   (define order
;;     (~>
;;      (hash->list)
;;      (sort (λ (x y) (> (cddr x) (cddr y))))
;;      (filter (λ (x) (pair? (car x))))

  
;;   (define (aux items acc)
;;     (cond
;;       [(pair? items)
;;        (let ([item (car items)])
;;          (if (pair? item)

  
;  (sort (hash->list final) (λ (x y) (string<? (car x) (car y)))))
  
             
; )

(define reg
  (~>
   (functor->register input)
   ))

(define (compile-program lists)
  (let-values
      ([(seen prog)
        (for/fold ([seen (make-immutable-hash)]
                   [prog (list)])
                  ([l lists])
          (wdb "~a" l)
          (if (pair? (car l))
              (begin
                (wdb "is list")
                (for/fold ([seen2 seen]
                           [prog2 (cons
                                    `(get-structure ,(car (hash-ref reg (car l))))
                                    prog)])
                          ([i (cdar l)])
                  (if (hash-has-key? seen2 i)
                      (values
                       seen2
                       (cons `((unify-value ,(car (hash-ref reg i)))) prog2 ))
                      (values
                       (hash-set seen2 i i)
                       (cons `((unify-variable ,(car (hash-ref reg i)))) prog2)))))

              (begin
                (wdb "is val")
                (cond
                  [(char-lower-case? (string-ref (symbol->string (car l)) 0))
                   (values seen  (cons `(get-structure ,(car (hash-ref reg (car l)))) prog))]
                  [(hash-has-key? seen l)
                   (values seen (cons `((unify-value ,(car (hash-ref reg (car l))))) prog))]
                  [else
                    (values
                     (hash-set seen l l)
                     (cons `((unify-variable ,(car (hash-ref reg (car l))))) prog))]
                     ))))
        ])
                
    (reverse prog)))

(~>
 reg
 (hash->list)
 (sort (λ (x y) (< (cddr x) (cddr y))))
 (filter (λ (x) (or (pair? (car x)) (char-lower-case? (string-ref (symbol->string (car x)) 0)))) _ )
 (compile-program))


(~>
 reg
 (hash->list)
 (sort (λ (x y) (> (cddr x) (cddr y))))
 (filter (λ (x) (pair? (car x))) _ )
 (append-map
  (λ (x)
    (cons `(put-structure ,(car (hash-ref reg (car x))))
          (append-map
           (λ (y)
             `((set-variable ,(car (hash-ref reg y))))) (cdar x) ))) _))
    






    
    ;; (for/fold ([out (make-immutable-hash)]  ; final registers
    ;;            [temp (make-immutable-hash)] ; term => register mapping
    ;;            [i   1])                     ; max register
    ;;           ([v f])
    ;;   ; if v exists already
    ;;   (if (hash-has-key? temp v)
          
    ;;   (values out temp i)))
      
    
    
  

(define-syntax-rule (do-print x ...)
    (printf x ...))



(define-syntax (namespace stx)
  (syntax-parse stx
    [(_ namespace-name:str exprs ...)
     (with-syntax
       ([(x ...)
         (map (λ (s) (syntax-property s 'ns (syntax-e #'namespace-name)))
              (syntax->list #'(exprs ...)))])
       (let ([e (local-expand #'(x ...)  'expression (list))])
         (writeln "exp")
         (writeln e)
         (define out (open-output-file #:exists 'append "c:\\temp\\temp.txt"))
         (write (syntax->list e) out)
         (display "\r\n" out)
         (close-output-port out)
        (datum->syntax stx e)))]))
;     #'(begin x ...))]))



(define-syntax (add stx)
  (syntax-parse stx
    [(_ a)
     ;; (write "add origin: ")
     ;; (writeln (syntax-property stx 'origin))
     ;; (write "ns : ")
     ;; (writeln (syntax-property stx 'ns))

     ;; (write "add pos: ")
     ;; (writeln (syntax-line stx))
     ;; (define out (open-output-file #:exists 'append "c:\\temp\\temp.txt"))
     ;; (write stx out)
     ;; (display "\r\n" out)
     ;; (close-output-port out)

     #''(+ a a)]))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ a)
     ;; (write "test origin: ")
     ;; (writeln (syntax-property stx 'origin))
     ;; (write "test pos: ")
     ;; (writeln (syntax-line stx))
     ;; (write "ns : ")
     ;; (writeln (syntax-property stx 'ns))

     (syntax/loc stx
        (add a))]))
  

(namespace "juan"
           {test 10}
           (test 20))


;; (define (annotate input)
;;   (define gen-name
;;     (let ([n -1])
;;       (λ (a)
;;         (set! n (add1 n))
;;         (format "t~a" n))))
;;   (define (aux input env)
;;     (match input
;;       [(list 'param (? string? name))
;;        (match env
;;          [(hash-table ((? string? name) value))
;;           (values (list (list 'param name value)) env)]
;;          [_
;;           (let* ([v (gen-name #f)]
;;                  [env (hash-set env name v)])
;;             (values (list (list 'param name v)) env))])]       

;;       [(list sym args ...)
;;        (let-values
;;            ([(args env)
;;              (for/fold ([new-args '()][e env])
;;                        ([arg args])
;;                (let-values ([(arg e) (aux arg e)])
;;                  (values (append arg new-args) e)))])         
;;          (values `((,sym ,@(reverse args) ,(gen-name #f))) env))]))  
;;     (aux input (hash)))


;; (define (gen-constraints input acc)
;;   (match input
;;     [(list (and (or 'int 'bool 'string) s) t)
;;      (hash-set acc (list t (symbol->string s)) #f)]
;;     [(list 'param x t) acc]
;;     [(list 'app f x t)
;;      ; f : x -> t
;;      (let* ([acc (hash-set acc (list (last f) (last x) t) #f)]
;;             [acc (gen-constraints f acc)]
;;             [acc (gen-constraints x acc)])
;;        acc)]
;;     [(list 'lambda f x t)
;;      ; t : f -> x
;;      (let* ([acc (hash-set acc (list t (last f) (last x)) #f)]
;;             [acc (gen-constraints f acc)]
;;             [acc (gen-constraints x acc)])
;;        acc)]
;;     [(list 'if pe te fe t)
;;      (let* ([acc (hash-set* acc
;;                             (list (last pe) "bool") #f
;;                             (list (last te) t) #f
;;                             (list (last fe) t) #f)]
;;             [acc (gen-constraints pe acc)]
;;             [acc (gen-constraints te acc)]
;;             [acc (gen-constraints fe acc)])
;;        acc)]))

;; (define (bound-type? input)
;;   (case input
;;     [("bool") #t]
;;     [("int") #t]
;;     [else #f]))



;; (define (unify constraints)
;;   (let ([sorted
;;          (sort (map car (hash->list constraints))
;;                (λ (x y)
;;                    (match (list x y)
;;                      [(list (list t (? bound-type?))
;;                             (list t2 (? bound-type?)))
;;                       (string<? t t2)]                   
;;                      [(list (list t (? bound-type?)) _) #t]
;;                      [(list _ (list t (? bound-type?))) #f]
;;                      [(list (list-rest t _)
;;                             (list-rest t2 _ ))
;;                       (cond
;;                         [(> (length x) (length y)) #f]
;;                         [(> (length y) (length x)) #t]
;;                         [else   (string<? t t2)])])))])
;;     (for/fold ([env (hash)])
;;               ([c sorted])
;;       ;; (match c
;;       ;;   [(list n (? bound-type? t))
;;       ;;    (when (not hash-has-key? env 
         
;;       env)

    
;;     ))
                    

;; (define-values (ann env)
;;   (annotate '(lambda
;;                  (param "is10")
;;                (lambda
;;                    (param "x")
;;                  (if
;;                   (app (param "is10") (param "x"))
;;                   (int)
;;                   (int)
                  
;;                   )))))

;; (define con (gen-constraints (car ann) (hash)))

;; (unify con)


;; (require (for-syntax racket/match))
;; (define (test x)

;;   (for/fold ([env (hash)])
;;             ([c x])
;;     (define-match-expander hash-value2
;;       (λ (stx)
;;         (syntax-case stx ()
;;           [(_ v)
;;            #'(app partial-address-or-8bit (? identity v))
;;            ])))

;;     (define (hash-value k)
;;       (when (hash-has-key? env k)
;;         (hash-ref env k)))

;;     (match c
;;       [(app hash-value x)
;;        (writeln x)
;;        env
;;        ]
;;       [_
;;        (writeln #f)
;;        env])))

;; (test (list 1 2 3))
