#lang racket
(require syntax/parse/define)
(require (for-syntax syntax/parse
                     racket/string))
(require (for-syntax racket/set))
(require (for-syntax racket/format))
(require (for-syntax syntax/srcloc))
;(require racket/match)
(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require racket/trace)
(require racket/string)
(require (for-syntax racket/syntax))

;(string-join (cdr (string-split "hello.world.three" ".")) ".")

(require threading)
(require (for-syntax threading))

;(provide (except-out (all-from-out racket) #%app sort when if)
;(rename-out [app #%app])

(provide
 (except-out
  (all-defined-out)
  s-for
  s-sort
  s-when
  s-case
  s-unless
  s-if
  s-list
  s-return
  s-begin
  s-while
  s-and
  s-cond
;  s-shuffle
  ))

(provide #%module-begin
         #%top-interaction
         #%top)
(provide
 (rename-out
  [app #%app]
  [s-for for]
  [s-sort sort]
  [s-case case]
  [s-when when]
  [s-unless unless]
  [s-if if]
  [s-list list]
  [s-return return]
  [s-begin begin]
  [s-while while]
  [s-and and]
  [s-cond cond]
          ))         


(define-syntax-parser wdb
  ([_ msg args ...] #'(writeln (format msg args ...))))

(struct context
  (program
   max-assembled
   awaiting-functions
   awaiting-labels
   labels
   string-table
   max-string) #:mutable )
(define asm (context (make-vector (* 1024 1024) 0) 0 (make-hash) (list) (make-hash) (make-hash) -1))

(begin-for-syntax
  (define num (box 0))
  (define (new-label)    
    (let* ([n (unbox num)]
           [l (format "label~a:" n)])
      (set-box! num (+ n 1))
      (string->symbol l)))
  (define num2 (box 0))
  (define (new-var)    
    (let* ([n (unbox num2)]
           [v (format "var~a" n)])
      (set-box! num2 (+ n 1))
      v)))

(define (new-string-index)
  (set-context-max-string! asm (+ 1 (context-max-string asm)))
  (context-max-string asm))

(define (new-label-index)
  (for/fold ([acc 0])
            ([i (hash-keys (context-labels asm))])
    (if (> i acc) acc i)))

(define (string-index s)
  (if (hash-has-key? (context-string-table asm) s)
      ( hash-ref (context-string-table asm) s)
      (let ([index  (new-string-index)])
        (hash-set! (context-string-table asm) s index)
        index)))
         
(define (write-vec-byte b v)
  (let ([m (context-max-assembled asm)])
    (vector-set! v m b)
;    (wdb "maxasm++")
    (set-context-max-assembled! asm (+ 1 m))))

(define (grow-vector)
  (let* ([v (context-program asm)]
         [len (vector-length v)])
    (when (= (context-max-assembled asm) len)
      (wdb "grow")
      (let ([newv (make-vector (* 2 len))])
        (vector-copy! newv 0 v )
        (set-context-program! asm newv)))))
      
(define (check-string in)
  (cond
    [(string? in) 
      (string-index in)]
    [(is-label? in)
     ;(wdb "setting label target ~a ~a" in (context-max-assembled asm))
      (set-context-awaiting-labels! asm
       (cons
        (cons (symbol->string in) (context-max-assembled asm))
        (context-awaiting-labels asm)))
      0]
    [else in]))


(define (get-int-bytes in)
  (let ([in (case in
              [(#t) 1]
              [(#f) 0]
              [else in])])    
    (let ([a (arithmetic-shift (bitwise-and in #xFF000000) -24)]
          [b (arithmetic-shift (bitwise-and in #xFF0000) -16)]
          [c (arithmetic-shift (bitwise-and in #xFF00) -8)]
          [d (bitwise-and in #xFF)])
      (list a b c d))))

  
(define (write-int in out)
  (for ([b (get-int-bytes in)])
    (if (vector? out)
        (write-vec-byte b out)
        (write-byte b out))))

(define (is-label? s)
 (and (symbol? s)
      (string-suffix? (symbol->string s) ":")))

(define (get-bytes input)
 ;(wdb "~a : ~a" (context-max-assembled asm) input)
  (let* ([next
          (match input
            [(list-rest (? is-label? (app symbol->string lab) ) xs)
             (hash-set! (context-labels asm) lab (context-max-assembled asm)) 
             xs             ]
            [a a])])
    (unless (empty? next)
      (match next
        [(list 'brk) 0]
        [(list 'pop) 1]
        [(list 'ldval x)    (flatten (list 2 (get-int-bytes(check-string x))))]
        [(list 'ldvals x)    (flatten (list 3 (get-int-bytes(check-string x))))]
        [(list 'ldvalb x)    (flatten (list 4 (get-int-bytes(check-string x))))]
        [(list 'ldvar x)    (flatten (list 5 (get-int-bytes(check-string x))))]
        [(list 'stvar x)    (flatten (list 6 (get-int-bytes(check-string x))))]
        [(list 'p_stvar x)    (flatten (list 7 (get-int-bytes(check-string x))))]
        [(list 'rvar x)    (flatten (list 8 (get-int-bytes(check-string x))))]
        [(list 'ldprop) 9]
        [(list 'p_ldprop) 10]
        [(list 'stprop) 11]
        [(list 'p_stprop) 12]
        [(list 'inc) 13]
        [(list 'dec) 14]
        [(list 'add) 15]
        [(list 'sub) 16]
        [(list 'mul) 17]
        [(list 'div) 18]
        [(list 'mod) 19]
        [(list 'rndi) 20]
        [(list 'startswith) 21]
        [(list 'p_startswith) 22]
        [(list 'endswith) 23]
        [(list 'p_endswith) 24]
        [(list 'contains) 25]
        [(list 'p_contains) 26]
        [(list 'indexof) 27]
        [(list 'p_indexof) 28]
        [(list 'substring) 29]
        [(list 'p_substring) 30]
        [(list 'ceq) 31]
        [(list 'cne) 32]
        [(list 'cgt) 33]
        [(list 'cgte) 34]
        [(list 'clt) 35]
        [(list 'clte) 36]
        [(list 'beq x)    (flatten (list 37 (get-int-bytes(check-string x))))]
        [(list 'bne x)    (flatten (list 38 (get-int-bytes(check-string x))))]
        [(list 'bgt x)    (flatten (list 39 (get-int-bytes(check-string x))))]
        [(list 'blt x)    (flatten (list 40 (get-int-bytes(check-string x))))]
        [(list 'branch x)    (flatten (list 41 (get-int-bytes(check-string x))))]
        [(list 'createobj) 42]
        [(list 'cloneobj) 43]
        [(list 'getobj) 44]
        [(list 'getobjs) 45]
        [(list 'delprop) 46]
        [(list 'p_delprop) 47]
        [(list 'delobj) 48]
        [(list 'moveobj) 49]
        [(list 'p_moveobj) 50]
        [(list 'createlist) 51]
        [(list 'appendlist) 52]
        [(list 'p_appendlist) 53]
        [(list 'prependlist) 54]
        [(list 'p_prependlist) 55]
        [(list 'removelist) 56]
        [(list 'p_removelist) 57]
        [(list 'len) 58]
        [(list 'p_len) 59]
        [(list 'index) 60]
        [(list 'p_index) 61]
        [(list 'keys) 62]
        [(list 'values) 63]
        [(list 'syncprop) 64]
        [(list 'getloc) 65]
        [(list 'genloc) 66]
        [(list 'genlocref) 67]
        [(list 'setlocsibling) 68]
        [(list 'p_setlocsibling) 69]
        [(list 'setlocchild) 70]
        [(list 'p_setlocchild) 71]
        [(list 'setlocparent) 72]
        [(list 'p_setlocparent) 73]
        [(list 'getlocsiblings) 74]
        [(list 'p_getlocsiblings) 75]
        [(list 'getlocchildren) 76]
        [(list 'p_getlocchildren) 77]
        [(list 'getlocparent) 78]
        [(list 'p_getlocparent) 79]
        [(list 'setvis) 80]
        [(list 'p_setvis) 81]
        [(list 'adduni) 82]
        [(list 'deluni) 83]
        [(list 'splitat) 84]
        [(list 'shuffle) 85]
        [(list 'sort) 86]
        [(list 'sortby) 87]
        [(list 'genreq) 88]
        [(list 'addaction) 89]
        [(list 'p_addaction) 90]
        [(list 'suspend) 91]
        [(list 'cut) 92]
        [(list 'say) 93]
        [(list 'pushscope) 94]
        [(list 'popscope) 95]
        [(list 'lambda x)    (flatten (list 96 (get-int-bytes(check-string x))))]
        [(list 'apply) 97]
        [(list 'ret) 98]
        [(list 'dbg) 99]
        [(list 'dbgl) 100]

                ))))

(define (assemble opcodes)
  (for ([opcode opcodes])
    (unless (empty? opcode)
      (if
       (list? (car opcode)) 
       (assemble opcode)
       (let ([code (get-bytes opcode)]
             [v (context-program asm)])
         (cond
           [(list? code) (for ([b code]) (write-vec-byte b v))]
           ; might be void if it was just a label
           [(not (void? code)) (write-vec-byte code v)]
           ))))))


(define (fully-assemble data)
  (writeln "attempting to assemble..")
  ;program
  (assemble data)
  ;(wdb "assmebled")
  ; go and fix labels
  (for ([targ (context-awaiting-labels asm)])
    (let ([source (cdr targ)]
          [dest (hash-ref (context-labels asm) (car targ))])
      ;(wdb "label ~a source ~a dest ~a" targ source dest)      
      (for ([i (in-naturals)]
            [b (get-int-bytes (- dest source))])
;        (wdb "offset ~a" b)
        (vector-set!
         (context-program asm)
         (+ 1 source i)
         b
         )))))
    
  
(define (write-file loc data)
  (fully-assemble data)
    ; string table is written as a 32 bit int number of strings, followed by
  ; each string prefixed with its length
  ;(wdb "labels ~a" (context-labels asm))
  ;(wdb "awaiting labels ~a" (context-awaiting-labels asm))
  (define out (open-output-file loc  #:exists 'replace #:mode 'binary))
  (write-int (hash-count (context-string-table asm)) out)
  ;(wdb "string table")
  (for ([kvp (sort (hash->list (context-string-table asm)) < #:key cdr)])
    ;(wdb "string order ~a" kvp)
    (let* ([v (car kvp)]
           [l (string-length v)])
      (write-int l out)
      (for ([c v])
        (write-char c out))))
  ;(wdb "finished")
  ;entry point

  ;(wdb "writing entry point ~a" (hash-ref (context-labels asm) "main:"))
  (write-int (hash-ref (context-labels asm) "main:")  out)
  ;(wdb "program")

  (let ([v (vector-take (context-program asm) (context-max-assembled asm))])
    ;(wdb "~a" (vector-length v))
    (for ([b v])      
      (write-byte b out)))
  (close-output-port out)
  (wdb "finished"))




(begin-for-syntax
  (define scoped-bindings-stack (box (list (mutable-set))))
  (define (push-scoped-stack)
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cons (mutable-set) lst)])
      (set-box! scoped-bindings-stack new-lst)))
    
  (define (pop-scoped-stack)
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cdr lst)])
      (set-box! scoped-bindings-stack new-lst)))

  (define (peek-scoped-stack)
    (let ([lst (unbox scoped-bindings-stack)])
      (car lst)))
            
  (define (add-scoped-binding stx-name stx)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (when (set-member? scoped name)
        (writeln
         (format "warning: ~a is already in scope at ~a"
                 name (source-location->string stx))))
      (set-add! scoped name)))

  (define (remove-scoped-binding stx-name)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (set-remove! scoped name)))

  (define (in-scope? name)
    (define (aux lst)
      (cond
        [(equal? name "global") #t]
        [(empty? lst) #f]
        [(set-member? (car lst) name) #t]
        [else (aux (cdr lst))]))
    (aux (unbox scoped-bindings-stack))))
    

(begin-for-syntax
  (define-syntax-class scoped-binding
    #:description "identifier in scope"
    #:opaque
    (pattern x:id
             #:with name  (symbol->string (syntax-e #'x))
             #:when (in-scope? (symbol->string (syntax-e #'x)))
             )))

(begin-for-syntax
  (define-syntax-class binding
    #:description "identifier name"
    #:opaque
    (pattern x:id
             #:with name (symbol->string (syntax-e #'x))
             )))


(begin-for-syntax
  (define-syntax-class prop-accessor
    #:description "property accessor"
    (pattern x:id
             #:when (string-contains? (symbol->string (syntax-e #'x)) ".")
             #:with ident1 (car (string-split (symbol->string (syntax-e #'x)) "."))
             #:with ident (string->symbol (car (string-split (symbol->string (syntax-e #'x)) ".")))
             #:when (in-scope?  (syntax-e #'ident1))
             #:with prop (cadr (string-split (symbol->string (syntax-e #'x)) "."))
             )))


(define-syntax (say-client stx)
  (syntax-parse stx
    [(_ arg msg)
     #'`((,(eval-arg arg)
          ,(eval-arg msg)
          (say)))]))

(define-syntax (list-len stx)
  (syntax-parse stx
    [(_ lst)
     #'`((,(eval-arg lst)
          (len)
          ))]))

(define-syntax (create-player-list stx)
  (syntax-parse stx
    [(_ var name items ... )
     #'`(
         (createlist)   ;create new list on stack
         ,(set-prop var name)
         ; now for each item, load the list and append
         ((,(get-prop var name)
           (ldvals items)
           (appendlist)) ...)
         )]))


(define-syntax (s-lambda stx)
  (syntax-parse stx
    [(_ ((arg ...)) body)
     ;tuple destructure
     (let*
         ([args (flatten (syntax->datum #'(arg ...)))]   
          [loaders         
           (for/list ([i (in-naturals)]
                      [s args])
             (with-syntax
               ([prop-name (string-append "item" (~a i))]
                [arg-name (symbol->string s)])
               #'((ldvals prop-name)
                  (p_ldprop)
                  (stvar arg-name))))])
       (with-syntax
         ([load (datum->syntax stx loaders)]
          [label (new-label)])
         (push-scoped-stack)
         
         #'`((pending-function             
              label
             load
             (pop)
             ,body
             ,(pop-scoped-stack)
             (ret))
           (lambda label))))
            
       ]
    [(_ (arg:binding) body)
     (push-scoped-stack)
     (add-scoped-binding #'arg.name stx)
     (with-syntax
       ([label (new-label)])
       #'`(           
           ;tell the assembler to create this later
           (pending-function             
             label 
             ;(stvar ,@(var-name arg))
             (stvar arg.name)
             ,body
             ,(pop-scoped-stack)
             (ret))
           (lambda label)
          )

       )]))
     
(define-syntax (s-for stx)
  (syntax-parse stx
    [(_ (var:binding list-expr) exprs ...)
     (with-syntax*
       ([label (new-label)]
        [continue (new-label)]
        [idx (new-var)]
        [start
         #'`(
             ,(eval-arg list-expr)
             ;test there are items otherwise skip
             (p_len)
             (ldval 0)
             (beq continue)
             (ldval 0)
             (stvar idx)
             (label ldvar idx)
             (p_index)
             (stvar var.name)
             )]
        [end
         #'`(
             (p_len)
             (ldvar idx)
             (inc)
             (p_stvar idx)
             (bne label)
             (continue)
             (pop)
             ,(pop-scoped-stack)
             )])
       (push-scoped-stack)
       (add-scoped-binding #'var.name stx)
       #'(start exprs ... end))]))


(define-syntax-parser pop-scoped-stack
  [(_ )
   (pop-scoped-stack)
   #''()])

(define-syntax-parser push-scoped-stack
  [(_)
   (push-scoped-stack)
   #'`()])
  
(define-syntax (push-binding stx)
  (syntax-parse stx
    [(_ id)
     (add-scoped-binding #'id stx)
  #'`(())]))
    
(define-syntax (for-reverse stx)
  (syntax-parse stx
    [(_ (var list-expr) exprs ...)
     (with-syntax*
       ([label (new-label)]
        [continue (new-label)]
        [id (symbol->string (syntax-e #'var))]
        [idx (new-var)]
        [start
         #'`(
             ,(eval-arg list-expr)
             ;test there are items otherwise skip
             (p_len)
             (ldval 0)
             (beq continue)
             (p_len)
             (dec)
             (stvar idx)
             (label ldvar idx)
             (p_index)
             (stvar id)
             )]
        [end
         #'`(
             (ldval -1)
             (ldvar idx)
             (dec)
             (p_stvar idx)
             (bne label)
             (continue)
             (pop)
             )])
       #'(start  exprs ... end))]))
     
(define-syntax (get-prop stx)
  (syntax-parse stx
    [(_ obj key)
     #'`(,(eval-arg obj)
         ,(eval-arg key)
         (ldprop))]))

(define-syntax (del-prop stx)
  (syntax-parse stx
    [(_ obj key)
     #'`(,(eval-arg obj)
         ,(eval-arg key)
         (delprop))]))

(define-syntax (set-prop stx)
  (syntax-parse stx
    [(_ obj key val)
     #'`(,(eval-arg obj)
         ,(eval-arg key)
         ,(eval-arg val)
         (stprop))]))

(define-syntax (set-props stx)
  (syntax-parse stx
    [(_ obj ([key val]...))
     (with-syntax*
       ([id (symbol->string (syntax-e #'obj))])
       #'`(,(eval-arg obj)
           ((,(eval-arg key)
             ,(eval-arg val) 
             (p_stprop))...)
           (pop)
           ))]))

(define-syntax (get-state stx)
  (syntax-parse stx
    [(_)
     #''((ldval -1)
         (getobj))]))

(define-syntax (get-global stx)
  (syntax-parse stx
    [(_ key)
     #'`((ldval -1)
         (getobj)
         ,(eval-arg key)
         (ldprop))]))

(define-syntax (global-obj stx)
  (syntax-parse stx
    [(_)
     #'`((ldval -1)
         (getobj))]))

(define-syntax (set-global stx)
  (syntax-parse stx
    [(_ key value)
     #'`((ldval -1)
         (getobj)
         ,(eval-arg key)
         ,(eval-arg value)
         (stprop))]))

(define-syntax (get-players stx)
  (syntax-parse stx
    [(_)
     #'`(,(get-state)
         (ldvals "players")
         (ldprop))]))

(define-syntax (var-name stx)
  (syntax-parse stx
    [(_ name)
     (with-syntax ([name  (symbol->string (syntax-e #'name))])
       #'name)]))

(define-syntax (tuple stx)
  ;not sure how to do this nicely for n-tuples yet so ill just do
  ; it manually for a few for now
  (syntax-parse stx
    [(_ a b)
     #'`((createobj)         
         (ldvals "item0")
         ,(eval-arg a)
         (p_stprop)
         (ldvals "item1")
         ,(eval-arg b)
         (p_stprop))]
    [(_ a b c)
     #'`((createobj)         
         (ldvals "item0")
         ,(eval-arg a)
         (p_stprop)
         (ldvals "item1")
         ,(eval-arg b)
         (p_stprop)
         (ldvals "item2")
         ,(eval-arg c)
         (p_stprop))]))
         
(define-syntax (def stx)
  (syntax-parse stx
    [(_ (names ...) expr )
     ;tuple extraction
     (let*
         ([args (flatten (syntax->datum #'(names ...)))]          
          [loaders         
           (for/list ([i (in-naturals)]
                      [s args])
             (with-syntax
               ([prop-name (string-append "item" (~a i))]
                [arg-name (symbol->string s)])
               #'((ldvals prop-name)
                  (p_ldprop)
                  (stvar arg-name))))])
       (with-syntax ([loadd (datum->syntax stx loaders)])
       #'`(,(eval-arg expr)
           loadd
           (pop))))]
             
    [(_ id:binding expr)
     (add-scoped-binding #'id.name stx)
     (syntax/loc stx
     `( ;expr should calculate some value on the stack, or load a constant
       ,(eval-arg expr)
       (stvar id.name)))]))

(define-syntax (new-list stx)
  (syntax-parse stx
    [(_ exprs ... ) #''((createlist))]))



(define-syntax-parser eval-arg
;  (wdb "eval arg ~a" this-syntax)
  [(_ id:prop-accessor)
   #'(get-prop id.ident id.prop)]  
  [(_ id:scoped-binding)   
     ; if this is an identifier then it will be a string table lookup
     ; to a variable       
   (syntax/loc this-syntax '((ldvar id.name)))]
  [(_ expr:str)     
   #''((ldvals expr))]
  [(_ expr:integer)
   #''((ldval expr))]
  [(_ expr:boolean)
   #''((ldvalb expr))]
  [(_ expr)
   ;otherwise evaluate it
   #'`,expr ])

(define-syntax-parser append-list
  [(_ var exprs ... )
   #'`(((,(eval-arg var)
         ,(eval-arg exprs)
         (appendlist)) ...))])

(define-syntax-parser prepend-list
  [(_ var exprs ... )
   #'`(((,(eval-arg var)
         ,(eval-arg exprs)
         (prependlist)) ...))])

(define-syntax-parser keys 
  [(_ expr)
   #'`(((,(eval-arg expr)
         (keys))))])

(define-syntax-parser vals
  [(_ expr)
   #'`(((,(eval-arg expr)
         (values))))])
         
(define-syntax-parser s-list  
  [(_) #''(createlist)]
  [(_ expr ...)
   #'`((createlist)
       ((,(eval-arg expr)
         (p_appendlist)) ...)
       )]) 

(define-syntax-parser def-obj
  [(_ id:binding)
   (add-scoped-binding #'id.name this-syntax)
   #'`((createobj)
       (stvar id.name))]
  [(_ id:binding ([key value]...))
   (add-scoped-binding #'id.name this-syntax)
   #'`(,(create-obj ([key value] ...))
       (stvar id.name))])

(define-syntax-parser s-sort
  [(_ l) #'`(,(eval-arg l)
             (ldvalb 0)
             (sort))])

(define-syntax-parser sortDesc
  [(_ l) #'`(,(eval-arg l)
             (ldvalb 1)
             (sort))])

(define-syntax-parser sortBy
  [(_ l k) #'`(,(eval-arg l)
               (ldvalb 0)
               ,(eval-arg k)
             (sortby))])

(define-syntax-parser sortByDesc
  [(_ l k) #'`(,(eval-arg l)
               (ldvalb 1)
               ,(eval-arg k)
               (sortby))])

(define-syntax-parser shuffle
  [(_ l) #'`(,(eval-arg l)
               (shuffle))])

(define-syntax (create-obj stx)
  (syntax-parse stx
    [(_)
     #'`((createobj)
         )]
    [(_ ([key value]...))
     #'`((createobj)
         ((,(eval-arg key)
           ,(eval-arg value)
           (p_stprop))...)           
         )]))

(define-syntax (move-obj stx)
  (syntax-parse stx    
    [(_ obj location)
     #'`(,(eval-arg obj)
         ,(eval-arg location)
         (moveobj))]))
          
(define-syntax-parser def-flow
  [(_ name:binding title)
   (add-scoped-binding #'name.name this-syntax)
   #'`(,(eval-arg title)
       (genreq)
       (stvar name.name))])

(define-syntax-parser add-flow-action
  [(_ req id text)
   #'`(,(eval-arg req)
       ,(eval-arg text)
       ,(eval-arg id)
       (addaction)
       )])

(define-syntax-parser flow-end
  [(_) #'`((cut))])


(begin-for-syntax
  (define-splicing-syntax-class flow-clauses
    [pattern {~seq [eq-expr case-title-expr true-expr] ...}
             #:with [n ...] (map ~a (range (length (attribute eq-expr))))]))

(define-syntax-parser flow
  [(_ client title-expr (expr:flow-clauses))
   (with-syntax
     ([flow-name (string->symbol (new-var))]
      [resp-name (string->symbol (new-var))])
     (add-scoped-binding #'flow-name this-syntax)
     (add-scoped-binding #'resp-name this-syntax)
   #'`(,(def-flow flow-name title-expr)
       ((,(s-when expr.eq-expr
          (add-flow-action flow-name expr.n expr.case-title-expr))...))  
       ,(def resp-name (flow flow-name client))
       ,(s-cond
          [(eq resp-name expr.n) expr.true-expr] ...)
       ))]
  [(_ req client)
   #'`(,(eval-arg req)
       ,(eval-arg client)
       (suspend))])


(define-syntax-parser remove-list
  [(_ var key)
   #'`(,(eval-arg var)
       ,(eval-arg key)
       (removelist))])

(define-syntax-parser remove-uni
  [(_ var)
   #'`(,(eval-arg var)
       (deluni)
       )])

(define-syntax (s-return stx)
  (syntax-parse stx
    [(_ var)
     #'`(,(eval-arg var))]))

(define-syntax (call stx)
  (syntax-parse stx
    [(_ (func args ...))
     (with-syntax
       ([func (string->symbol (string-append (symbol->string (syntax-e #'func)) ":"))]
        [(rargs ...) (datum->syntax stx (reverse (syntax->datum #'(args ...))))]
        )
       #'`(,(eval-arg rargs) ...
           (call func)))]))

(define-syntax (create-location stx)
  (syntax-parse stx
    [(_ location-name)
     #'`(,(eval-arg location-name)
         (genloc))]
    [(_ location-name parent-location)
     #'`(,(eval-arg location-name)
         (genloc)
         ,(eval-arg parent-location)
         (genlocref)
         (p_setlocparent))]
    ))

(define-syntax (link-location stx)
  (syntax-parse stx
    [(_ host new-sibling ([key value]...))
     #'`(,(eval-arg host)
         ,(eval-arg new-sibling)         
         (genlocref)         
         ((,(eval-arg key)
           ,(eval-arg value)
           (p_stprop))...)           
         (setlocsibling))]))

(define-syntax (get-siblings stx)
  (syntax-parse stx
    [(_ location)
     #'`(,(eval-arg location)
         (getlocsiblings))]))

(define-syntax (get-children stx)
  (syntax-parse stx
    [(_ location)
     #'`(,(eval-arg location)
         (getlocchildren))]))

(define-syntax (get-loc stx)
  (syntax-parse stx
    [(_ location)
     #'`(,(eval-arg location)
         (getloc))]))

(define-syntax (nth stx)
  (syntax-parse stx
    [(_ n lst)
     #'`(,(eval-arg lst)
         ,(eval-arg n)
         (index))]))

(define-syntax (dbg stx)
  (syntax-parse stx
    [(_ var ...)
     #'`((,(eval-arg var)
          (dbg)) ...)]))

(define-syntax (dbgl stx)
  (syntax-parse stx
    [(_ var ... last-var)
     #'`(((,(eval-arg var)
           (dbg)) ...)
         ,(eval-arg last-var)
         (dbgl))]))

(define-syntax (s-if stx)
  (syntax-parse stx
    [(_ expr true-expr false-expr)
     (with-syntax ([else-label (new-label)]
                   [end-label (new-label)])
       #'`((,(eval-arg expr)
            (ldvalb 0)
            (beq else-label)
            ,(eval-arg true-expr)
            (branch end-label)
            (else-label)
            ,(eval-arg false-expr)
            (end-label))))]))

(define-syntax (s-unless stx)
  (syntax-parse stx
    [(_ expr true-expr)
     (with-syntax ([label (new-label)])
       #'`((,(eval-arg expr)
            (ldvalb 1)
            (beq label)
            ,(eval-arg true-expr)
            (label))))]))

(define-syntax (s-when stx)
  (syntax-parse stx
    [(_ expr true-expr ...)
     (with-syntax ([label (new-label)])
       #'`((,(eval-arg expr)
            (ldvalb 0)
            (beq label)
            ,(eval-arg true-expr) ...
            (label))))]))

(define-syntax (s-while stx)
  (syntax-parse stx
    [(_ cond-expr body-expr ... )
     (with-syntax ([start (new-label)]
                   [end (new-label)])
       #'`(((start)
            ,(eval-arg cond-expr)
            (ldvalb 0)
            (beq end)
            ,(eval-arg body-expr) ...
            (branch start)
            (end))))]))

(define-syntax (eq stx)
  (syntax-parse stx
    [(_ left right)
     #'`(,(eval-arg left)
         ,(eval-arg right)
         (ceq))]))

(define-syntax (ne stx)
  (syntax-parse stx
    [(_ left right)
     #'`(,(eval-arg left)
         ,(eval-arg right)
         (cne))]))

(define-syntax (gt stx)
  (syntax-parse stx
    [(_ left right)
     #'`(,(eval-arg right)
         ,(eval-arg left)
         (cgt))]))

(define-syntax (gte stx)
  (syntax-parse stx
    [(_ left right)
     #'`(,(eval-arg right)
         ,(eval-arg left)
         (cgte))]))

(define-syntax (lt stx)
  (syntax-parse stx
    [(_ left right)
     #'`(
         ,(eval-arg right)
         ,(eval-arg left)
         (clt))]))

(define-syntax (lte stx)
  (syntax-parse stx
    [(_ left right)
     #'`(
         ,(eval-arg right)
         ,(eval-arg left)
         (clte))]))

(define-syntax (add stx)
  (syntax-parse stx
    [(_ left right ...)
     #'`(,(eval-arg left)
         ((,(eval-arg right)
         (add)) ...))]))

(define-syntax (sub stx)
  (syntax-parse stx
    [(_ left right)
     #'`(,(eval-arg left)
         ,(eval-arg right)
         (sub))]))

(define-syntax (starts-with stx)
  (syntax-parse stx
    [(_ str test)
     #'`(,(eval-arg str)
         ,(eval-arg test)
         (startswith))]))

(define-syntax (ends-with stx)
  (syntax-parse stx
    [(_ str test)
     #'`(,(eval-arg str)
         ,(eval-arg test)
         (endswith))]))

(define-syntax (index-of stx)
  (syntax-parse stx
    [(_ str test)
     #'`(,(eval-arg str)
         ,(eval-arg test)
         (indexof))]))

(define-syntax (substring stx)
  (syntax-parse stx
    [(_ str start len)
     #'`(,(eval-arg str)
         ,(eval-arg start)
         ,(eval-arg len)
         (substring))]))

(define-syntax (contains stx)
  (syntax-parse stx
    [(_ obj str)
     #'`(,(eval-arg obj)
         ,(eval-arg str)
         (contains))]))

(define-syntax (ignore stx)
  (syntax-parse stx
    [(_ expr ...)
     #'`((,(eval-arg expr)
         (pop)) ...)]))

(define-syntax (rndi stx)
  (syntax-parse stx
    [(_ max)
     #'`(,(eval-arg max)
         (ldval 0)
         (rndi))]
    [(_ min max)
     #'`(,(eval-arg max)
         ,(eval-arg min)
         (rndi))]))

(define-syntax (s-begin stx)
  (syntax-parse stx
    [(_ exprs ...)
     #'`( ,exprs ...)]))

(define-syntax (get-objs stx)
  (syntax-parse stx
    [(_ loc )
     #'`(,(eval-arg loc)
         (getobjs)
         )]))

(define-syntax (s-or stx)
  (syntax-parse stx
    [(_ expr ...)
     (with-syntax*
       ([when-true (new-label)]
        [end (new-label)]
        [(cases ...)
         #'`((,(eval-arg expr)
              (ldvalb 1)
              (beq when-true))...)]) 
       #'`(,(cases ...)
           ;push false on the stack
           (ldvalb 0)
           (branch end)
           ;if any of the cases are true they will
           ;jump here and push true on the stack
           (when-true ldvalb 1)
           (end)))]))

(define-syntax-parser s-and
  [(_ expr ...)
   (with-syntax*
     ([when-false (new-label)]
      [end (new-label)]
      [(cases ...)
       #'`((,(eval-arg expr)
            (ldvalb 0)
            (beq when-false))...)])
     #'`(,(cases ...)
         (ldvalb 1)
         (branch end)
         (when-false ldvalb 0)
         (end)))])

(define-syntax-parser s-not 
    [(_ expr )
     (with-syntax ([true-label (new-label)]
                   [end (new-label)])
     #'`(,(eval-arg expr)
         (ldvalb 1)
         (beq true-label)
         (ldvalb 1)
         (branch end)
         (true-label)
         (ldvalb 0)
         (end)))])

(define-syntax-parser s-cond-case
  [(_ test-expr then-body cond-end)
   #:with end (new-label)
   #'`(,(eval-arg test-expr)
       (ldvalb 1)
       (bne end)
       ,(eval-arg then-body)
       (branch cond-end)
       (end))])
            
(define-syntax-parser s-cond #:datum-literals (else)
    [(_ [test-expr then-body] ...+
        [else else-body])
     #:with cond-end (new-label)
     #'`(,(s-cond-case test-expr then-body cond-end) ...
           ,(eval-arg else-body)
           (cond-end))]
    [(_ [test-expr then-body] ...+)
     #:with cond-end (new-label)
       #'`(,(s-cond-case test-expr then-body cond-end) ...
           (cond-end))])     

(define-syntax-parser prop-match-id
  ([_ id]
   #:with new-id (symbol->string (syntax-e #'id))
   #'new-id))

(define-syntax (prop-match-val stx)
  (syntax-parse stx #:datum-literals (*)
    [(_ * end)   
      #''()]
    [(_ v-expr end)
     #'`(,(eval-arg v-expr)
         (bne end))]))

(define-syntax (has-key-value stx)
  (syntax-parse stx
    [(_ obj-expr key-expr val-expr)
     (with-syntax ([x (new-var)]
                   [end (new-label)]
                   [f-end (new-label)])
     #'`(,(eval-arg obj-expr)
         ,(eval-arg key-expr)
         (p_stvar x)
         (p_contains)
         (ldvalb 0)
         (beq f-end)
         (ldvar x)
         (ldprop)
         ,(eval-arg val-expr)
         (ceq)
         (branch end)
         (f-end)
         (ldvalb 0)
         (pop)
         (end)))]))                             

(define-syntax (obj-match stx)
  (syntax-parse stx 
    [(_ obj-expr
        ([(k v)
          (k-expr (~optional v-expr:expr #:defaults([v-expr #'*])))]...)
        body-expr        
        (~optional else-expr:expr #:defaults([else-expr #'`()]))
        )
     (with-syntax
       ([end (new-label)]
        [f-end (new-label)])
     #'`(
         ; for each case in turn, try to match both the key and
         ; value (if passed)  and save them in the passed variable names.
         ; if all match ok then execute the body.
         ; (note this can leave some variables populated even if there
         ; is no match - this is alright for now. it will aslo overwrite
         ; another variable with the same name. also ok.)
         (,(eval-arg obj-expr)
          ,(eval-arg k-expr)
          (p_stvar ,(prop-match-id k))
          (p_contains)
          (ldvalb 0)
          (beq f-end)
          (ldvar ,(prop-match-id k))
          (p_ldprop)
          (p_stvar ,(prop-match-id v))
          ,(prop-match-val v-expr f-end)) ...
         ,body-expr
         (branch end)
         (f-end)
         ,else-expr 
         (end)
         (pop)
         ))]))


(define-syntax (++ stx)
  (syntax-parse stx
    [(_ var)  (syntax/loc stx (set-var var (add 1 var)))]))

(define-syntax (+= stx)
  (syntax-parse stx
    [(_ var n)  (syntax/loc stx (set-var var (add n var)))]))

(define-syntax-parser prop+=
  [(_ obj key n)
   #'`(,(eval-arg obj) ;not very nice
       ,(eval-arg key)     
       ,(eval-arg obj)
       ,(eval-arg key)     
       (ldprop)
       ,(eval-arg n)
       (add)
       (stprop)
       )])

(define-syntax-parser prop-=
  [(_ obj key n)
   #'`(,(eval-arg obj) ;not very nice
       ,(eval-arg key)     
       ,(eval-arg n)
       ,(eval-arg obj)
       ,(eval-arg key)     
       (ldprop)
       (sub)
       (stprop)
       )])

(define-syntax (-- stx)
  (syntax-parse stx
    [(_ var)  #'(def var (sub 1 var))]))

(define-syntax-parser ~
  [(_ f args ...)
   #'`(,(eval-arg f)
       ((,(eval-arg args) (apply)) ...))])

(define-syntax-parser scurry
  [(_ exprs ... )
   ;; (writeln "original")
   ;; (writeln (syntax->datum #'(exprs ...)))
   (let* ([e (local-expand #'('(main:) (exprs ...) '(ret)) 'expression (list))])
     (define (folder input acc funcs)
       (define-values (a b)
         (for/fold
             ([acc acc]
              [funcs funcs])
             ([node input])
           (cond
             [(list? node)
              (match node
                [(list-rest '#%app (or 'list 'list*) (list 'quote 'pending-function) tail)
                 (let-values ([(a b) (folder tail null null)])
                   ;todo: handle the nested lambdas (or does it already??)
                   (values acc (cons b (cons a funcs))))]               
                [(list
                  '#%app
                  (or 'list* 'list)
                  (list 'quote (and (or 'stvar 'p_stvar 'ldvar 'p_ldvar) op))
                  (list 'quote x))
                 (values (cons (list op x) acc) funcs)]
                
                [_
                 (let-values ([(a b) (folder node null null)])
                   (values (cons a acc) (cons b funcs)))])      
              ]
             ;drop all this stuff we arent interested in
             [(eq? node '#%app) (values acc funcs)]
             [(eq? node 'list*) (values acc funcs)]
             [(eq? node 'list) (values acc funcs)]
             [(eq? node 'quote) (values acc funcs)]
             [else (values (cons node acc) funcs)])
           ))
       (values (reverse a) (reverse b)))

     (define-values (a b) (folder (syntax->datum e) null null))          
     (with-syntax ([yay (datum->syntax this-syntax (append a b))])
       #'(write-file "c:\\temp\\test.scur" 'yay))
     )     
   ]

  )

(define-syntax-parser λ
  [(_ (arg args ...+ ) body ...+)     
   #'(s-lambda (arg) (s-return (λ (args ...) body ...)))]
  [(_ (arg) body ...+)
   #'(s-lambda (arg) (s-begin body ...))]
  [(_ body)
   #'(λ (_) body)]
  ;; [(_ (body ...))
  ;;  #'(λ (_) (body ...))]
  )
  

(define-syntax-parser def-λ
  [(_ (name args ...+) body ...)   
   #'(def name (λ (args ...)
  ;               (dbgl "in " name)
                 body ...
                 ;              (dbgl "out " name)
                 ))]
  ; sugar a no-args lambda with _
  ; auto application calls this with #f
  ; which is ignored, so we dont have to
  ; have a speical unit() value
  [(_ (name) body ...)   
   #'(def name (λ (_)
;                 (dbgl "in " name)
                 body ...
 ;                (dbgl "out " name)
                 ))])

(define-syntax-parser import
    [(_ lib) #'lib])

(begin-for-syntax
  (define-splicing-syntax-class bindings #:datum-literals (:)
    [pattern
     ({~seq dest (~optional (~seq : key) #:defaults ([key #'dest])) } ...)
     #:post
     (~and
      (~parse             
       (key-str ...)
       (map (λ (x) (~>> x syntax-e symbol->string))
            (syntax->list #'(key ...))))
      (~parse             
       (dest-str ...)
       (map (λ (x) (~>> x syntax-e symbol->string))
            (syntax->list #'(dest ...)))))]))

(define-syntax (extract stx)
  (syntax-parse stx
    [(_ ([binding:bindings input-expr] ... ) body ...)
     #'`((
          (( (,(push-binding binding.dest-str) ...)
            ,(eval-arg input-expr)
            (((ldvals binding.key-str)
              (p_ldprop)
              (stvar binding.dest-str)) ...)
            (pop))  ...))
         ,body ...)]))


(define-syntax (s-match stx)
  ; todo: would be nicer withouht this λ (eq #t #t)
  (define-syntax-class match-exp #:datum-literals (_)
    (pattern (~or (~and _ (~bind [new-ex #'(λ (eq #t #t))]))
                  (~and ex:expr (~bind [new-ex #'(λ ex)])))))  
  (syntax-parse stx
    [(_ (arg-expr ...)
        [ ex:match-exp ... true-exp  ] ...)
     #' (s-cond
         [(s-and (~ ex.new-ex arg-expr) ...) true-exp] ...)]))

(define-syntax-parser extract-match
  [(_ ([binding:bindings input-expr] ... ) body ...)
   #'`((
        (((,(push-binding binding.dest-str) ...)
          ,(eval-arg input-expr)
          (((ldvals binding.key-str)
            (p_ldprop)
            (stvar binding.dest-str)) ...)
          (pop))  ...))
       ,(s-match (binding.dest ... ...) body ... ))])


(define-syntax-parser split-at
  [(_ n bottom inputs)
   #'`(,(eval-arg inputs)
       ,(eval-arg n)
       ,(eval-arg bottom)
       (splitat))])

(define-syntax-parser pop-list
  [(_ inputs)
   #'`(,(eval-arg inputs)
       (p_len)
       (dec)
       (ldvalb 0)
       (splitat))])

(define-syntax-parser assert
  [(_ condition msg ... )
   #'(s-unless condition (dbgl "assert failed : " msg ...))])

(define-syntax-parser clone-obj
  [(_ obj)
   #'`(,(eval-arg obj)
       (cloneobj))])

(define-syntax-parser set-var
  [(_ id:scoped-binding value)
   #'`(,(eval-arg value)
       (rvar id.name))
       ])

; useful for announcing in plae changes to lists
(define-syntax-parser sync-prop
  [(_ obj prop )
   #'`(,(eval-arg obj)
       ,(eval-arg prop)
       (syncprop))])

(define-syntax-parser s-case #:datum-literals (else)
  [(_ test [val expr ...] ...+
      [else else-expr ...])
   #'`(,(s-cond
         [(eq test val)
          (s-begin expr ...)] ...
          [else else-expr ... ]))]
   
    [(_ test [val expr ...] ... )
   #'`(,(s-cond
         [(eq test val)
          (s-begin expr ...)] ...))
       
   ])

(define-syntax (app stx)
  (syntax-parse stx
    #:datum-literals (<- != = += -= < <= > >= ++)
    [(_ f:prop-accessor += val)
     #'(prop+= f.ident f.prop val)]
    [(_ f:prop-accessor ++)
     #'(++ f)]
    [(_ f:prop-accessor -= val)
       #'(prop-= f.ident f.prop val)]
    [(_ f:prop-accessor = val)
     #'(eq (get-prop f.ident f.prop) val)]
    [(_ f = val)
     #'(eq f val)]
    [(_ f:prop-accessor <- val)
     #'(set-prop f.ident f.prop val)]
    [(_ f:scoped-binding <- val)
     #'(set-var f val)]
    [(_ f:prop-accessor < val)
     #'(lt f val)]
    [(_ f:prop-accessor > val)
     #'(gt f val)]
    [(_ f:prop-accessor <= val)
     #'(lte f val)]
    [(_ f:prop-accessor >= val)
     #'(gte f val)]
    [(_ f:prop-accessor != val)
     #'(ne f val) ]
    ; process everything else as scurry function app
    [(app f a ...+)
     #'(~ f  a ...)]
    [(app f)
     ;since we don't support unit we just "sugar" a
     ;call with no args with #fm sane as the def- λ
     #'(~ f #f)]
    ))



