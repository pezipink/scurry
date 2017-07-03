#lang racket
(require (for-syntax syntax/parse))
(require racket/match)
(require racket/trace)
(require racket/string)
(require (for-syntax racket/syntax))
(provide (all-defined-out))
(define-syntax (wdb stx)
  (syntax-parse stx
    ([_ msg args ...] #'(writeln (format msg args ...)))))

(struct context (program max-assembled awaiting-labels labels string-table max-string) #:mutable )
(define asm (context (make-vector (* 1024 1024) 0) 0 (list) (make-hash) (make-hash) -1))

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
     ;(wdb "setting label target ~a" in)
      (set-context-awaiting-labels! asm
       (cons
        (cons (symbol->string in) (context-max-assembled asm))
        (context-awaiting-labels asm)))
      0]
    [else in]))


(define (get-int-bytes in)
  (let ([a (arithmetic-shift (bitwise-and in #xFF000000) -24)]
        [b (arithmetic-shift (bitwise-and in #xFF0000) -16)]
        [c (arithmetic-shift (bitwise-and in #xFF00) -8)]
        [d (bitwise-and in #xFF)])
    (list a b c d)))
  
(define (write-int in out)
  (for ([b (get-int-bytes in)])
    (if (vector? out)
        (write-vec-byte b out)
        (write-byte b out))))

(define (is-label? s)
 (and (symbol? s)
      (string-suffix? (symbol->string s) ":")))

(define (get-bytes input)
  (wdb "~a" input)
  (let* ([next
          (match input
            [(list-rest (? is-label? (app symbol->string lab) ) xs)
             (hash-set! (context-labels asm) lab (context-max-assembled asm)) 
             xs             ]
            [a a])])
    (unless (empty? next)
      (match next
        [(list 'pop) 0]
        [(list 'ldval x)    (flatten (list 1 (get-int-bytes(check-string x))))]
        [(list 'ldvals x)    (flatten (list 2 (get-int-bytes(check-string x))))]
        [(list 'ldvalb x)    (flatten (list 3 (get-int-bytes(check-string x))))]
        [(list 'ldvar x)    (flatten (list 4 (get-int-bytes(check-string x))))]
        [(list 'stvar x)    (flatten (list 5 (get-int-bytes(check-string x))))]
        [(list 'p_stvar x)    (flatten (list 6 (get-int-bytes(check-string x))))]
        [(list 'ldprop) 7]
        [(list 'p_ldprop) 8]
        [(list 'stprop) 9]
        [(list 'p_stprop) 10]
        [(list 'hasprop) 11]
        [(list 'p_hasprop) 12]
        [(list 'add) 13]
        [(list 'sub) 14]
        [(list 'mul) 15]
        [(list 'div) 16]
        [(list 'mod) 17]
        [(list 'rndi) 18]
        [(list 'concat) 19]
        [(list 'cstr) 20]
        [(list 'cint) 21]
        [(list 'ceq) 22]
        [(list 'cne) 23]
        [(list 'cgt) 24]
        [(list 'clt) 25]
        [(list 'beq x)    (flatten (list 26 (get-int-bytes(check-string x))))]
        [(list 'bne x)    (flatten (list 27 (get-int-bytes(check-string x))))]
        [(list 'bgt x)    (flatten (list 28 (get-int-bytes(check-string x))))]
        [(list 'blt x)    (flatten (list 29 (get-int-bytes(check-string x))))]
        [(list 'branch x)    (flatten (list 30 (get-int-bytes(check-string x))))]
        [(list 'createobj) 31]
        [(list 'cloneobj) 32]
        [(list 'getobj) 33]
        [(list 'getobjs) 34]
        [(list 'delprop) 35]
        [(list 'p_delprop) 36]
        [(list 'delobj) 37]
        [(list 'moveobj) 38]
        [(list 'p_moveobj) 39]
        [(list 'createlist) 40]
        [(list 'appendlist) 41]
        [(list 'p_appendlist) 42]
        [(list 'removelist) 43]
        [(list 'p_removelist) 44]
        [(list 'len) 45]
        [(list 'p_len) 46]
        [(list 'index) 47]
        [(list 'p_index) 48]
        [(list 'keys) 49]
        [(list 'values) 50]
        [(list 'getloc) 51]
        [(list 'genloc) 52]
        [(list 'genlocref) 53]
        [(list 'setlocsibling) 54]
        [(list 'p_setlocsibling) 55]
        [(list 'setlocchild) 56]
        [(list 'p_setlocchild) 57]
        [(list 'setlocparent) 58]
        [(list 'p_setlocparent) 59]
        [(list 'getlocsiblings) 60]
        [(list 'p_getlocsiblings) 61]
        [(list 'getlocchildren) 62]
        [(list 'p_getlocchildren) 63]
        [(list 'getlocparent) 64]
        [(list 'p_getlocparent) 65]
        [(list 'setvis) 66]
        [(list 'p_setvis) 67]
        [(list 'adduni) 68]
        [(list 'deluni) 69]
        [(list 'roll) 70]
        [(list 'deal) 71]
        [(list 'shuffle) 72]
        [(list 'merge) 73]
        [(list 'sort) 74]
        [(list 'genreq) 75]
        [(list 'addaction) 76]
        [(list 'p_addaction) 77]
        [(list 'suspend) 78]
        [(list 'suspendu) 79]
        [(list 'fallback) 80]
        [(list 'say) 81]
        [(list 'call x)    (flatten (list 82 (get-int-bytes(check-string x))))]
        [(list 'ret) 83]
        [(list 'dbg) 84]
        [(list 'dbgl) 85]

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

(define (write-file loc data)
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
        (vector-set!
         (context-program asm)
         (+ 1 source i)
         b
         ))))
    
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

(define-syntax (scurry stx)
  (syntax-parse stx
    [(_ exprs ... )     
     #'(begin
         (let ([program (append exprs ... )])
           (write-file "c:\\temp\\test.scur" program)))]))

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

(define-syntax (foreach stx)
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
             (ldval 0)
             (stvar idx)
             (label ldvar idx)
             (p_index)
             (stvar id)
             )]
        [end
         #'`(
             (p_len)
             (ldval 1)  ; increase  loop index
             (ldvar idx)
             (add)
             (p_stvar idx)
             (bne label)
             (continue)
             (pop)
             )])
       #'(append start (append exprs ...) end))]))
     
(define-syntax (get-prop stx)
  (syntax-parse stx
    [(_ obj key)
     #'`(,(eval-arg obj)
         ,(eval-arg key)
         (ldprop))]))

(define-syntax (has-prop stx)
  (syntax-parse stx
    [(_ obj key)
     #'`(,(eval-arg obj)
         ,(eval-arg key)
         (hasprop))]))

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

(define-syntax (def stx)
  (syntax-parse stx
    [(_ (name args ...) body ...)
     (with-syntax
       ([func (string->symbol (string-append (symbol->string (syntax-e #'name)) ":"))])
       ;function definition.
       #'`((func) ; label
           ; pop values off the stack and save to locals
           ((stvar ,(var-name args)) ...)
           ,body ...
           (ret)))]
    [(_ name expr)
     (with-syntax*
       ([id (symbol->string (syntax-e #'name))])
     #'`( ;expr should calculate some value on the stack, or load a constant
         ,(eval-arg expr)
         (stvar id)))]))

(define-syntax (new-list stx)
  (syntax-parse stx
    [(_ exprs ... ) #''((createlist))]))

(define-syntax (eval-arg stx)
  (syntax-parse stx
    [(_ expr:id)
     (with-syntax
       ([id (symbol->string (syntax-e #'expr))])
       ; if this is an identifier then it will be a string table lookup
       ; to a variable       
       #''((ldvar id)))]
    [(_ expr:str)     
     #''((ldvals expr))]
    [(_ expr:integer)
     #''((ldval expr))]
    [(_ expr:boolean)
     #''((ldvalb expr))]
    [(_ expr)
     ;otherwise evaluate it
     #'`,expr ]))

(define-syntax (append-list stx)
  (syntax-parse stx
    [(_ var exprs ... )
     #'`(
        ((,(eval-arg var)
          ,(eval-arg exprs)
          (appendlist)) ...))]))

(define-syntax (keys stx)
  (syntax-parse stx
    [(_ expr)
     #'`(
        ((,(eval-arg expr)
          (keys))))]))

(define-syntax (vals stx)
  (syntax-parse stx
    [(_ expr)
     #'`(
        ((,(eval-arg expr)
          (values))))]))
         
(define-syntax (def-list stx)
  (syntax-parse stx    
    [(_ name exprs ...)
     (with-syntax*
       ([id (symbol->string (syntax-e #'name))])
     #'`((createlist)
         (stvar id)         
         ,(append-list name exprs ...)))]))

(define-syntax (def-obj stx)
  (syntax-parse stx
    [(_ name)
     (with-syntax*
       ([id (symbol->string (syntax-e #'name))])
       #'`((createobj)
           (stvar id)))]
    [(_ name ([key value]...))
     (with-syntax*
       ([id (symbol->string (syntax-e #'name))])
       #'`(,(create-obj ([key value] ...))
           (stvar id)))]))

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
          
(define-syntax (def-req stx)
  (syntax-parse stx
    [(_ name title)
     (with-syntax
       ([id (symbol->string (syntax-e #'name))])
       #'`(,(eval-arg title)
           (genreq)
           (stvar id)))]))

(define-syntax (add-req-action stx)
  (syntax-parse stx
    [(_ req id text)
     #'`(,(eval-arg req)
         ,(eval-arg text)
         ,(eval-arg id)
           (addaction)
           )]))

(define-syntax (suspend stx)
  (syntax-parse stx
    [(_ req client)
     #'`(,(eval-arg req)
         ,(eval-arg client)
         (suspend))]))

(define-syntax (remove-list stx)
  (syntax-parse stx
    [(_ var key)
     #'`(,(eval-arg var)
         ,(eval-arg key)
         (removelist))]))

(define-syntax (s-return stx)
  (syntax-parse stx
    [(_ var)
     #'`(,(eval-arg var))]))

;; (begin-for-syntax
;;   (define-syntax (reverse-args stx)
;;                        (datum->syntax stx (reverse (cdr (syntax->datum stx))))))

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
;     childlocref :: parentloc :: newloc
     #'`(
         ;generate new location
         ,(eval-arg location-name)
         (genloc)
         ;load parent location
         ,(eval-arg parent-location)
         ;create new link 
         (genlocref)
         (p_setlocparent))]
    ))

(define-syntax (link-location stx)
  (syntax-parse stx
    [(_ host new-sibling ([key value]...))
     ; locref :: sibling :: host
     #'`(;first the host goes on the stack
         ,(eval-arg host)
         ;next the new sibling
         ,(eval-arg new-sibling)
         ;create location ref and its kvps
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
     #'`((
          (,(eval-arg var)
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
     #'`(,(eval-arg left)
         ,(eval-arg right)
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

(define-syntax (ignore stx)
  (syntax-parse stx
    [(_ expr)
     #'`(,(eval-arg expr)
         (pop))]))

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

(define-syntax (suspend-dispatch stx)
  (syntax-parse stx
    [(_ req client ([test-expr body-expr]...))
     (with-syntax*
       ([var (new-var)]
        [end (new-label)]
        [(dispatch-case ...)
         (with-syntax
           ([skip-body (new-label)])
           #'`((;load the response
                (ldvar var)
                ;evaluate the match expression
                ,(eval-arg test-expr)
                ;skip the body if not equal
                (bne skip-body)
                ; otherwise run the body and
                ; jump to the end of the dispatcher 
                ,(eval-arg body-expr)
                (branch end)
                (skip-body))...))])       
       #'`(,(eval-arg req)
           ,(eval-arg client)
           (suspend)
           (stvar var)
           ,(dispatch-case ...)
           (end))
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

(define-syntax (s-not stx)
  (syntax-parse stx
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
         (end)))]))

(define-syntax (s-cond-case stx)
  (syntax-parse stx
     [(_ test-expr then-body cond-end)
      (with-syntax ([end (new-label)])
        #'`(,(eval-arg test-expr)
            (ldvalb 1)
            (bne end)
            ,(eval-arg then-body)
            (branch cond-end)
            (end)))]))
            
(define-syntax (s-cond stx)
  (syntax-parse stx #:datum-literals (else)                
    [(_ [test-expr then-body] ...+
        [else else-body])
     (with-syntax ([cond-end (new-label)])       
       #'`(,(s-cond-case test-expr then-body cond-end) ...
           ,(eval-arg else-body)
           (cond-end)))]
     [(_ [test-expr then-body] ...+)
      (with-syntax ([cond-end (new-label)])       
       #'`(,(s-cond-case test-expr then-body cond-end) ...
           (cond-end)))]))     

(define-syntax (prop-match-id stx)
  (syntax-parse stx
    ([_ id]
     (with-syntax ([new-id (symbol->string (syntax-e #'id))])
       #'new-id))))

(define-syntax (prop-match-val stx)
  (syntax-parse stx #:datum-literals (*)
    [(_ * end)   
      #''()]
    [(_ v-expr end)
     #'`(,(eval-arg v-expr)
         (bne end))])) 
    
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
         (p_hasprop)
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
(scurry
 (def (main)
   (dbgl (sub 1 10))))


;; (scurry
;;  (def (main)

;;    (def-obj juan
;;      (["key" 1]
;;       ["key2" 2]
;;       ["key3" "andrea!"]))

;;    (def p "andrea")
;;    (def x 2)

;;    (obj-match juan
;;      ([(_ v) ("key" 1)]
;;       [(k2 v2) ("key3" 20)])
;;       (s-begin
;;        (dbgl "key is " k " value is " v)
;;        (dbgl "key is " k2 " value is " v2))
;;       )
   
;;    ;; (def-req req "test")
;;    ;; (add-req-action req "a" "test a")
;;    ;; (add-req-action req "b" "test b")
;;    ;; (suspend-dispatch req "clienta"
;;    ;;   (["a" (dbg "client replied a")]
;;    ;;    ["b" (dbg "client replied b")]))
;;    ;; (def resp (suspend req "test request"))

;;    ))
