#lang racket
(require syntax/parse/define)
(require (for-syntax syntax/parse))
(require (for-syntax racket/set))
(require (for-syntax racket/format))
(require (for-syntax syntax/srcloc))
;(require racket/match)
(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require racket/trace)
(require racket/string)
(require (for-syntax racket/syntax))

(require threading)
(require (for-syntax threading))

(provide
 (except-out
  (all-defined-out)
  s-sort
  s-when
  s-if
  s-list
  s-return
  s-begin
  s-while
  s-and
;  s-shuffle
  ))

(provide
 (rename-out
  [s-sort sort]
  [s-when when]
  [s-if if]
  [s-list list]
  [s-return return]
  [s-begin begin]
  [s-while while]
  [s-and and]
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
;  (wdb "~a : ~a" (context-max-assembled asm) input)
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
        [(list 'ldprop) 8]
        [(list 'p_ldprop) 9]
        [(list 'stprop) 10]
        [(list 'p_stprop) 11]
        [(list 'inc) 12]
        [(list 'dec) 13]
        [(list 'add) 14]
        [(list 'sub) 15]
        [(list 'mul) 16]
        [(list 'div) 17]
        [(list 'mod) 18]
        [(list 'rndi) 19]
        [(list 'startswith) 20]
        [(list 'p_startswith) 21]
        [(list 'endswith) 22]
        [(list 'p_endswith) 23]
        [(list 'contains) 24]
        [(list 'p_contains) 25]
        [(list 'indexof) 26]
        [(list 'p_indexof) 27]
        [(list 'substring) 28]
        [(list 'p_substring) 29]
        [(list 'ceq) 30]
        [(list 'cne) 31]
        [(list 'cgt) 32]
        [(list 'clt) 33]
        [(list 'beq x)    (flatten (list 34 (get-int-bytes(check-string x))))]
        [(list 'bne x)    (flatten (list 35 (get-int-bytes(check-string x))))]
        [(list 'bgt x)    (flatten (list 36 (get-int-bytes(check-string x))))]
        [(list 'blt x)    (flatten (list 37 (get-int-bytes(check-string x))))]
        [(list 'branch x)    (flatten (list 38 (get-int-bytes(check-string x))))]
        [(list 'createobj) 39]
        [(list 'cloneobj) 40]
        [(list 'getobj) 41]
        [(list 'getobjs) 42]
        [(list 'delprop) 43]
        [(list 'p_delprop) 44]
        [(list 'delobj) 45]
        [(list 'moveobj) 46]
        [(list 'p_moveobj) 47]
        [(list 'createlist) 48]
        [(list 'appendlist) 49]
        [(list 'p_appendlist) 50]
        [(list 'removelist) 51]
        [(list 'p_removelist) 52]
        [(list 'len) 53]
        [(list 'p_len) 54]
        [(list 'index) 55]
        [(list 'p_index) 56]
        [(list 'keys) 57]
        [(list 'values) 58]
        [(list 'syncprop) 59]
        [(list 'getloc) 60]
        [(list 'genloc) 61]
        [(list 'genlocref) 62]
        [(list 'setlocsibling) 63]
        [(list 'p_setlocsibling) 64]
        [(list 'setlocchild) 65]
        [(list 'p_setlocchild) 66]
        [(list 'setlocparent) 67]
        [(list 'p_setlocparent) 68]
        [(list 'getlocsiblings) 69]
        [(list 'p_getlocsiblings) 70]
        [(list 'getlocchildren) 71]
        [(list 'p_getlocchildren) 72]
        [(list 'getlocparent) 73]
        [(list 'p_getlocparent) 74]
        [(list 'setvis) 75]
        [(list 'p_setvis) 76]
        [(list 'adduni) 77]
        [(list 'deluni) 78]
        [(list 'splitat) 79]
        [(list 'shuffle) 80]
        [(list 'sort) 81]
        [(list 'sortby) 82]
        [(list 'genreq) 83]
        [(list 'addaction) 84]
        [(list 'p_addaction) 85]
        [(list 'suspend) 86]
        [(list 'cut) 87]
        [(list 'say) 88]
        [(list 'pushscope) 89]
        [(list 'popscope) 90]
        [(list 'lambda x)    (flatten (list 91 (get-int-bytes(check-string x))))]
        [(list 'apply) 92]
        [(list 'ret) 93]
        [(list 'dbg) 94]
        [(list 'dbgl) 95]
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
;      (writeln "pushing new scope")
      (set-box! scoped-bindings-stack new-lst)))
    
  (define (pop-scoped-stack)
    (let* ([lst (unbox scoped-bindings-stack)]
           [new-lst (cdr lst)])
;      (writeln "popping scope")
      (set-box! scoped-bindings-stack new-lst)))

  (define (peek-scoped-stack)
    (let ([lst (unbox scoped-bindings-stack)])
  ;    (writeln (format "lst is ~a" lst))
      (car lst)))
            
  (define (add-scoped-binding stx-name stx)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
      (when (set-member? scoped name)
        (writeln
         (format "warning: ~a is already in scope at ~a"
                 name (source-location->string stx))))
;      (writeln (format "adding ~a to scoped bindings" name))  
      (set-add! scoped name)))

  (define (remove-scoped-binding stx-name)
    (let ([name (syntax-e stx-name)]
          [scoped (peek-scoped-stack)])
;      (writeln (format "removed binding ~a from scope " name))
      (set-remove! scoped name)))

  (define (in-scope? name)
    (define (aux lst)
;         (writeln (format "aux ~a ~a" lst name))
      (cond  
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
             ,(pop-scope)
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
             ,(pop-scope)
             (ret))
           (lambda label)
          )

       )]))
     
(define-syntax (foreach stx)
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
             ,(pop-scope)
             )])
       (push-scoped-stack)
       (add-scoped-binding #'var.name stx)
       #'(start exprs ... end))]))

(define-syntax (pop-scope stx)
  (syntax-parse stx
    [(_ )
     (pop-scoped-stack)
  #'`(())]))
(define-syntax (push-scope stx)
  (syntax-parse stx
    [(_ )
     (push-scoped-stack)
  #'`(())]))
  
(define-syntax (push-binding stx)
  (syntax-parse stx
    [(_ id)
     (add-scoped-binding #'id stx)
  #'`(())]))
    
(define-syntax (foreach-reverse stx)
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
    [(_ a b)
     #'`((createobj)         
         (ldvals "item0")
         ,(eval-arg a)
         (p_stprop)
         (ldvals "item1")
         ,(eval-arg b)
         (p_stprop)
         (ldvals "item2")
         ,(eval-arg a)
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
   #'`((createobj)
       (stvar id.name))]
  [(_ id:binding ([key value]...))
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
   (add-scoped-binding #'name.name)
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
     (add-scoped-binding #'flow-name)
     (add-scoped-binding #'resp-name)
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

(define-syntax (lt stx)
  (syntax-parse stx
    [(_ left right)
     #'`(
         ,(eval-arg right)
         ,(eval-arg left)
         (clt))]))

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
    [(_ str test)
     #'`(,(eval-arg str)
         ,(eval-arg test)
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
                 
(define-syntax (s-and stx)
  (syntax-parse stx
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
           (end)))]))

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
    [(_ var)  (syntax/loc stx (def var (add 1 var)))]))

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
                 ;                 (writeln "helloo")
                 ;                  (writeln tail)
                 (let-values ([(a b) (folder tail null null)])
                   ;todo: handle the nested lambdas (or does it already??)
                   ;; (writeln "lambda process")
                   ;; (writeln a)
                   ;; (writeln "B")
                   ;; (writeln b)
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
              
              
              ;; (if (and (not (empty? node))
              ;;          (eq? (car node) '))
              ;;     (values acc (cons node funcs))


              ]
             ;drop all this stuff we arent interested in
             [(eq? node '#%app) (values acc funcs)]
             [(eq? node 'list*) (values acc funcs)]
             [(eq? node 'list) (values acc funcs)]
             [(eq? node 'quote) (values acc funcs)]
             [else (values (cons node acc) funcs)])
           ))
       (values (reverse a) (reverse b)))

     ;; (writeln "original2")
     ;; (writeln (syntax->datum e))
     ;; (writeln "")
     ;       (writeln "res")
     (define-values (a b) (folder (syntax->datum e) null null))     
     ;; (writeln a)
     ;; (writeln "")
     ;; (writeln "res2")
     ;; (writeln b)
     ;; (writeln "")
     ;(writeln (datum->syntax stx  a))
     ;  (writeln (syntax->datum e)))
     
     (with-syntax ([yay (datum->syntax this-syntax (append a b))])
       #'(write-file "c:\\temp\\test.scur" 'yay))
     )     
   ]

  )

(define-syntax-parser λ
  [(_ (arg args ...+ ) body ...+)     
   #'(s-lambda (arg) (s-return (λ (args ...) body ...)))]
  [(_ (arg) body ...+)
   #'(s-lambda (arg)(s-begin body ...))]
  [(_ (body ...))
   #'(λ (_) (body ...))])

(define-syntax-parser def-λ
  [(_ (name args ...) body ...)   
   #'(def name (λ (args ...)
                 body ...))])

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

; useful for announcing in plae changes to lists
(define-syntax-parser sync-prop
  [(_ obj prop )
   #'`(,(eval-arg obj)
       ,(eval-arg prop)
       (syncprop))])
