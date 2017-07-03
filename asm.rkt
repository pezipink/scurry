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
        [(list 'add) 11]
        [(list 'sub) 12]
        [(list 'mul) 13]
        [(list 'div) 14]
        [(list 'mod) 15]
        [(list 'rndi) 16]
        [(list 'startswith) 17]
        [(list 'p_startswith) 18]
        [(list 'endswith) 19]
        [(list 'p_endswith) 20]
        [(list 'contains) 21]
        [(list 'p_contains) 22]
        [(list 'indexof) 23]
        [(list 'p_indexof) 24]
        [(list 'substring) 25]
        [(list 'p_substring) 26]
        [(list 'ceq) 27]
        [(list 'cne) 28]
        [(list 'cgt) 29]
        [(list 'clt) 30]
        [(list 'beq x)    (flatten (list 31 (get-int-bytes(check-string x))))]
        [(list 'bne x)    (flatten (list 32 (get-int-bytes(check-string x))))]
        [(list 'bgt x)    (flatten (list 33 (get-int-bytes(check-string x))))]
        [(list 'blt x)    (flatten (list 34 (get-int-bytes(check-string x))))]
        [(list 'branch x)    (flatten (list 35 (get-int-bytes(check-string x))))]
        [(list 'createobj) 36]
        [(list 'cloneobj) 37]
        [(list 'getobj) 38]
        [(list 'getobjs) 39]
        [(list 'delprop) 40]
        [(list 'p_delprop) 41]
        [(list 'delobj) 42]
        [(list 'moveobj) 43]
        [(list 'p_moveobj) 44]
        [(list 'createlist) 45]
        [(list 'appendlist) 46]
        [(list 'p_appendlist) 47]
        [(list 'removelist) 48]
        [(list 'p_removelist) 49]
        [(list 'len) 50]
        [(list 'p_len) 51]
        [(list 'index) 52]
        [(list 'p_index) 53]
        [(list 'keys) 54]
        [(list 'values) 55]
        [(list 'getloc) 56]
        [(list 'genloc) 57]
        [(list 'genlocref) 58]
        [(list 'setlocsibling) 59]
        [(list 'p_setlocsibling) 60]
        [(list 'setlocchild) 61]
        [(list 'p_setlocchild) 62]
        [(list 'setlocparent) 63]
        [(list 'p_setlocparent) 64]
        [(list 'getlocsiblings) 65]
        [(list 'p_getlocsiblings) 66]
        [(list 'getlocchildren) 67]
        [(list 'p_getlocchildren) 68]
        [(list 'getlocparent) 69]
        [(list 'p_getlocparent) 70]
        [(list 'setvis) 71]
        [(list 'p_setvis) 72]
        [(list 'adduni) 73]
        [(list 'deluni) 74]
        [(list 'roll) 75]
        [(list 'deal) 76]
        [(list 'shuffle) 77]
        [(list 'merge) 78]
        [(list 'sort) 79]
        [(list 'genreq) 80]
        [(list 'addaction) 81]
        [(list 'p_addaction) 82]
        [(list 'suspend) 83]
        [(list 'cut) 84]
        [(list 'say) 85]
        [(list 'call x)    (flatten (list 86 (get-int-bytes(check-string x))))]
        [(list 'ret) 87]
        [(list 'dbg) 88]
        [(list 'dbgl) 89]
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

(define-syntax (cut stx)
  (syntax-parse stx
    [(_)
     #'`((cut))]))

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
;; (scurry
;;  (def (main)
;;    (def test "hello world")
;;    (dbgl (starts-with test "hello"))
;;    (dbgl (ends-with test "hello"))
;;    (dbgl (ends-with test "world"))
;;    (dbgl (contains test "lo wo"))
;;    ))
