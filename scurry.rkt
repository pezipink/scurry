#lang racket
(require (for-syntax syntax/parse))
(require racket/match)
(require racket/trace)
(require racket/string)
(require (for-syntax racket/syntax))
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
      v))

  )

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
     (wdb "setting label target ~a" in)
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
        [(list 'stprop) 8]
        [(list 'p_stprop) 9]
        [(list 'hasprop) 10]
        [(list 'p_hasprop) 11]
        [(list 'add) 12]
        [(list 'sub) 13]
        [(list 'mul) 14]
        [(list 'div) 15]
        [(list 'mod) 16]
        [(list 'rndi) 17]
        [(list 'concat) 18]
        [(list 'cstr) 19]
        [(list 'cint) 20]
        [(list 'ceq) 21]
        [(list 'cne) 22]
        [(list 'cgt) 23]
        [(list 'clt) 24]
        [(list 'beq x)    (flatten (list 25 (get-int-bytes(check-string x))))]
        [(list 'bne x)    (flatten (list 26 (get-int-bytes(check-string x))))]
        [(list 'bgt x)    (flatten (list 27 (get-int-bytes(check-string x))))]
        [(list 'blt x)    (flatten (list 28 (get-int-bytes(check-string x))))]
        [(list 'branch x)    (flatten (list 29 (get-int-bytes(check-string x))))]
        [(list 'createobj) 30]
        [(list 'cloneobj) 31]
        [(list 'getobj) 32]
        [(list 'delprop) 33]
        [(list 'p_delprop) 34]
        [(list 'delobj) 35]
        [(list 'moveobj) 36]
        [(list 'p_moveobj) 37]
        [(list 'createlist) 38]
        [(list 'appendlist) 39]
        [(list 'p_appendlist) 40]
        [(list 'removelist) 41]
        [(list 'p_removelist) 42]
        [(list 'len) 43]
        [(list 'p_len) 44]
        [(list 'index) 45]
        [(list 'p_index) 46]
        [(list 'keys) 47]
        [(list 'values) 48]
        [(list 'getloc) 49]
        [(list 'genloc) 50]
        [(list 'genlocref) 51]
        [(list 'setlocsibling) 52]
        [(list 'p_setlocsibling) 53]
        [(list 'setlocchild) 54]
        [(list 'p_setlocchild) 55]
        [(list 'setlocparent) 56]
        [(list 'p_setlocparent) 57]
        [(list 'getlocsiblings) 58]
        [(list 'p_getlocsiblings) 59]
        [(list 'getlocchildren) 60]
        [(list 'p_getlocchildren) 61]
        [(list 'getlocparent) 62]
        [(list 'p_getlocparent) 63]
        [(list 'setvis) 64]
        [(list 'p_setvis) 65]
        [(list 'adduni) 66]
        [(list 'deluni) 67]
        [(list 'roll) 68]
        [(list 'deal) 69]
        [(list 'shuffle) 70]
        [(list 'merge) 71]
        [(list 'sort) 72]
        [(list 'genreq) 73]
        [(list 'addaction) 74]
        [(list 'p_addaction) 75]
        [(list 'suspend) 76]
        [(list 'suspendu) 77]
        [(list 'fallback) 78]
        [(list 'say) 79]
        [(list 'call x)    (flatten (list 80 (get-int-bytes(check-string x))))]
        [(list 'ret) 81]
        [(list 'dbg) 82]
        [(list 'dbgl) 83]
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
  (wdb "assmebled")
  ; go and fix labels
  (for ([targ (context-awaiting-labels asm)])
    (let ([source (cdr targ)]
          [dest (hash-ref (context-labels asm) (car targ))])
      (wdb "label ~a source ~a dest ~a" targ source dest)

      (for ([i (in-naturals)]
            [b (get-int-bytes (- dest source))])
        (vector-set!
         (context-program asm)
         (+ 1 source i)
         b
         ))))
    
    ; string table is written as a 32 bit int number of strings, followed by
  ; each string prefixed with its length
  (wdb "labels ~a" (context-labels asm))
  (wdb "awaiting labels ~a" (context-awaiting-labels asm))
  (define out (open-output-file loc  #:exists 'replace #:mode 'binary))
  (write-int (hash-count (context-string-table asm)) out)
  (wdb "string table")
  (for ([kvp (sort (hash->list (context-string-table asm)) < #:key cdr)])
    (wdb "string order ~a" kvp)
    (let* ([v (car kvp)]
           [l (string-length v)])
      (write-int l out)
      (for ([c v])
        (write-char c out))))
  (wdb "finished")
  ;entry point

  (wdb "writing entry point ~a" (hash-ref (context-labels asm) "main:"))
  (write-int (hash-ref (context-labels asm) "main:")  out)
  (wdb "program")

  (let ([v (vector-take (context-program asm) (context-max-assembled asm))])
    (wdb "~a" (vector-length v))
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
             ;; (ldval 2)  ; todo: swap this around and use less than
             ;; (add)
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
    [(_ var ...)
     #'`((,(eval-arg var)
           (dbgl)) ...)
         ]))

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

(define-syntax (while stx)
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

(scurry
 (def (main)
   (def-list colours "green" "blue" "orange" "yellow" "white")
   (set-global "race-on" 1)
   (call (init-locations))
   (call (setup-players))
   ;create the camels
   (def-obj camels)
   (def stack (get-prop (get-loc "track1") "camel-stack"))
   (foreach (c colours)
     (def-obj camel (["type" "camel"]
                     ["colour" c]))
     (move-obj camel (get-loc "track1"))
     (append-list stack camel)
     ;assign the camel to a quick-lookup dict by colour
     (set-prop camels c camel))
   (set-prop (get-loc "track1") "camel-stack" stack)
   (def camel-keys (keys camels))
   (while (get-global "race-on")
     (def i (rndi (list-len camel-keys)))
     (def spaces (rndi 1 3))
     (call (move-camel
            (get-prop camels (nth i camel-keys))
            spaces)))

   ; race over
   (foreach (c (vals camels))
     (dbg "camel " (get-prop c "colour") " is at location " (get-loc c))
     (dbgl ""))
   )
 
 

 (def (move-camel camel amount)
   (dbg "move-camel " (get-prop camel "colour") " " amount "\n")
   ; camels are stored in stack objects (lists) within a location
   ; under the camel-stack key.  the camel should be located and
   ; the stack split into two stacks - since the camels on top
   ; of the target camel get moved with it.  the new stack gets
   ; appended to the camel stack of the target location.

   ; first, work out the target destination by navigating through the
   ; "next" sibling of the locations.
   (def current-loc (get-loc camel))
   (def target-loc (get-loc camel))
   (def cnt 0)
   (while (lt cnt amount)
     ;todo: replace this with a find macro/func of some kind
     (def sibs (get-siblings target-loc))
     (def cnt2 0) 
     (while (eq 0 (has-prop (nth cnt2 sibs) "next"))
       (def cnt2 (add 1 cnt2)))
     (def sib (nth cnt2 sibs))
     (s-when (eq "finish" (get-prop sib "next"))
       (set-global "race-on" 0))
     (def target-loc (get-loc sib))
     (def cnt (add 1 cnt)))

   ; now we have the target location.
   (dbg "current-loc is " current-loc "\n")
   (dbg "target-loc is " target-loc "\n")
   ; grab the camel stack from the current location and split it
   ; such that the current camel and the rest of the list forms the new list
   (def current-stack (get-prop current-loc "camel-stack"))
   (def target-stack (get-prop target-loc "camel-stack"))
   (def-list new-current-stack)
   (def found-camel 0)
   (foreach (c current-stack)
     (s-when (eq c camel)
       (def found-camel 1))     
     (s-if (eq 1 found-camel)
       (s-begin
        (append-list target-stack c)
        (dbg "moving " (get-prop c "colour") " to " target-loc "\n")
        (move-obj c target-loc))
       ;else 
       (append-list new-current-stack c)))

   ;assign new lists
   (set-prop current-loc "camel-stack" new-current-stack)
   (set-prop target-loc "camel-stack" target-stack))
 
 (def (init-locations)
   (def root-loc (create-location "root"))

   ;entire track location
   (ignore (create-location "track" root-loc))
   ;first segment
   (def track (create-location "track1" "track"))
   (set-prop track "camel-stack" '(createlist))
   (def track-no 2)
   (while (lt track-no 17)
     ;create and link the next 15 sections
     (def curr-track (add "track" track-no))
     (def prev-track (add "track" (sub 1 track-no)))
     (def track (create-location curr-track "track"))
     (link-location curr-track prev-track (["prev" "prev"]))
     (link-location prev-track curr-track (["next" "next"]))
     (set-prop track "camel-stack" '(createlist))
     (def track-no (add 1 track-no)))
   ;wrap around at end, mark as finish line!
   (link-location "track16" "track1" (["next" "finish"]))

   (def pyramid-stack (create-location "pyramid-stack" root-loc))
   (def pyramid (create-location "pyramid" root-loc))

   ;betting card locations for the camels
   ;camel / dice tents
   (def tents-loc (create-location "tents-loc" root-loc))
   (def betting-loc (create-location "betting-loc" root-loc))

   (def-list amounts 2 3 5)
   (foreach (c colours)
     (def tent (create-location (add c "-tent") tents-loc))
     ;each tent has a coloured die       
     (def bet (create-location (add c "-bets") "betting-loc"))
     ;each betting location has 3 betting cards, 5 3 and 2.
     (def-list leg-cards)     
     (foreach (a amounts)
      (def card (create-obj
                 (["colour" c]
                  ["type" "leg-bet"]
                  ["owner" ""])))
      (move-obj card bet)
      (append-list leg-cards card))
     (set-prop tent "leg-cards" leg-cards)
     ;one pyramid card for each camel
     
     ))
   
 (def (setup-players)
   ; get player list
   (def players (vals (get-players)))   
   (foreach (player players)
    ; each player gets 8 egyptian pounds
    ; a set of camel cards and a trap
    (def client (get-prop player "clientid"))

    (def-list finish-cards)
    (foreach (c colours)
      ;one finish betting card for each colour camel!
      (append-list finish-cards
        (create-obj (["colour" c]
                     ["type" "camel-card"]
                     ["owner" client]))))

    (set-props
     player
     (["egyptian pounds" 8]
      ["trap" (create-obj (["type" "trap"]
                           ["owner" client]))]
      ["camel-cards" finish-cards]))
    
    ;debug text
    (dbg "player " client " now as the following props\n")
    (foreach (k (keys player))
      (dbg k " -> " (get-prop player k) "\n"))))
     

) 




;; (scurry
;;  ;functions!
;;  (def (set-role player role)
;;    (set-prop player "role" role))

;;  (def (main)
;;    (def-list roles "Diver" "Explorer" "Pilot" "Engineer" "Messenger" "Adventurer")
;;    (def players (vals (get-players)))
;;    ; each connected client chooses a role
;;    (foreach-list (player players)
;;     (def id (get-prop player "clientid"))
;;     (def-req req "choose a role")
;;     ;build a request message representing the remaining roles
;;     (foreach-list (v roles)
;;      (add-req-action req v v))

;;     ; suspend via json to client
;;     (def resp (suspend req id))
;;     ; this executes when the client responds
;;     (call (set-role player resp))
;;     ; remove this from the available roles
;;     (def roles (remove-list roles resp)))))






           
;; (scurry
;;  (def (main)
;;  (def-list roles "Diver" "Explorer" "Pilot" "Engineer" "Messenger" "Adventurer")
;;  (foreach-list
;;   (client (vals (get-players)))
;;   ; build message and ask client to pick a role;
;; ;  [say-client client "hello"]
;; ;  (def-msg msg "select a role" (foreach-list (role roles) role))
;; ;  (def resp (suspend (get-prop "clientid" client) msg))
;; ;  (set-prop resp)
;;   ;(remove-list roles resp)
;;   )
;;  )
;;  )
  
           
                 
               
