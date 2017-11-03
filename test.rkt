;#lang s-exp "asm.rkt"
#lang racket
(require racket/match)
(require (for-syntax racket/match))
(require (for-syntax syntax/parse))
(require racket/list)
(require threading)

(define-syntax (namespace stx)
  (syntax-parse stx
    [(_ namespace-name:str exprs ...)
     
     (syntax-property #'(begin exprs ...) 'ns (syntax-e #'namespace-name))]))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ a)
     (writeln (syntax-property stx 'origin))
     #'(writeln a)]))
  

(namespace "juan" (test (test 10))
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
