#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/format))
(require racket/match)
(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require racket/trace)
(require racket/string)
(require (for-syntax racket/syntax))
(provide (all-defined-out))
(require threading)
(define-syntax (wdb stx)
  (syntax-parse stx
    ([_ msg args ...] #'(writeln (format msg args ...)))))

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
  (wdb "~a : ~a" (context-max-assembled asm) input)
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
        [(list 'pushscope) 86]
        [(list 'popscope) 87]
        [(list 'lambda x )
         (flatten (list 88 (get-int-bytes(check-string x))))]
        [(list 'apply) 89]
        [(list 'p_apply) 90]
        [(list 'ret) 91]
        [(list 'dbg) 92]
        [(list 'dbgl) 93]
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
         #'`((pending-function             
             label 
             load
             ,body
             (ret))
           (lambda label))))
            
       ]
    [(_ (arg) body)
     (with-syntax
       ([label (new-label)])
       #'`(
           ;tell the assembler to create this later
           (pending-function             
             label 
             (stvar ,@(var-name arg))
             ,body
             (ret))
           (lambda label)
          )

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
       #'(start exprs ... end))]))

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
             (ldval -1)
             (add)
             (stvar idx)
             (label ldvar idx)
             (p_index)
             (stvar id)
             )]
        [end
         #'`(
             (ldval -1)
             (ldval 1)  ; decrease loop index
             (ldvar idx)

             (sub)
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
         
(define-syntax (s-list stx)
  (syntax-parse stx
    [(_) #''(createlist)]
    [(_ expr ...)
     #'`((createlist)
         ((,(eval-arg expr)
          (p_appendlist)) ...)
         )]))

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
          
(define-syntax (def-flow stx)
  (syntax-parse stx
    [(_ name title)
     (with-syntax
       ([id (symbol->string (syntax-e #'name))])
       #'`(,(eval-arg title)
           (genreq)
           (stvar id)))]))

(define-syntax (add-flow-action stx)
  (syntax-parse stx
    [(_ req id text)
     #'`(,(eval-arg req)
         ,(eval-arg text)
         ,(eval-arg id)
           (addaction)
           )]))

(define-syntax (flow-end stx)
  (syntax-parse stx
    [(_)
     #'`((cut))]))

(define-syntax (flow stx)
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

(define-syntax (flow-dispatch stx)
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
    [(_ expr ...)2
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
    [(_ var)  #'(def var (add 1 var))]))


(define-syntax (-- stx)
  (syntax-parse stx
    [(_ var)  #'(def var (sub 1 var))]))

(define-syntax (~ stx)
  (syntax-parse stx
    [(_ f args ...)
     #'`(,(eval-arg f)
         ((,(eval-arg args)
           (apply)) ...)
         )]))


(define-syntax (scurry stx)
  (syntax-parse stx
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
       
       (with-syntax ([yay (datum->syntax stx (append a b))])
         #'(write-file "c:\\temp\\test.scur" 'yay))
       )     
     ]

    ))

(define-syntax (λ stx)
  (syntax-parse stx
    [(_ (arg args ...+ ) body ...+)     
       #'(s-lambda (arg)
           (s-return
             (λ (args ...) body ...)))]
    [(_ (arg) body ...+)
        #'(s-lambda (arg)
            (s-begin
             body ...))]
    [(_ (body ...))
     #'(λ (_) (body ...))]))

(define-syntax (def-λ stx)
  (syntax-parse stx
    [(_ (name args ...) body ...)
     #'(def name (λ (args ...) body ...))]))

(define-syntax (import stx)
  (syntax-parse stx
    [(_ lib) #'lib]))
      

