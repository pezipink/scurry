#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     racket/format))

(scurry
 
 (import core-lib)
 (def players (vals (get-players)))

 ;fork/join
 (def-obj responses)
 (def-λ (get-choice id)
   (def a (flow id "make a choice" (["hello" "hello"]["goodbye" "goodbye"])))
   (def b (flow id "make another choice" (["andrea" "andrea"]["ross" "ross"])))
   (set-prop responses id (a + b))
   (flow-end))
 

 ;; (for (p players)  (fork get-choice p.clientid))

 ;; (dbgl "waiting on players")
 
 ;; (join) 
 
 ;; (for (p players)
 ;;   (dbgl "player " p.clientid " chose " (get-prop responses p.clientid)))
 
 ;; (dbgl "continuing")

 
 
;;  'brk
 ; list tests
 ; split-top removes n elements and creates
 ; a new list from them
 (def x (arr 1 2 3 4 5 6 7 8 ))

 (def y (split-top 3 x))

 ;pop should return a single value not a list
 (assert (is-int? (pop-arr x)))

;;  'brk

 (assert (arrs-same? (arr 1 2 3 4) x)
         "expected [1 2 3 4] but got " x)

 (assert (arrs-same? (arr 6 7 8) y)
         "expected [6 7 8] but got " y)

 ; split-bottom removes n elements from the
 ; front, creating a new list from them
 (def z (split-bottom 2 x))
 (assert (arrs-same? (arr 3 4) x)
         "expected [3 4] but got " x)

 (assert (arrs-same? (arr 1 2) z)
         "expected [1 2] but got " z)
 
 ; remove-list removes a single element
 ;; (remove-list x 4)
 ;; (assert (lists-same? (list 3) x)
 ;;         "expected [3] but got " x)

 ;; (global.hello <- "world")
 ;; (dbgl "hello global " global.hello)
 
 (def-obj function-table
   (["square" (λ (add _ _))]
    ["cube"   (λ (add _ _ _))]
    ["twice"  (λ (f x) (f (f x)))]))

 (dbgl "10 squared :" (function-table.square 10))
 (dbgl "10 cubed :" (function-table.cube 10))
 (dbgl "10 squared squared :"
       (function-table.twice function-table.square 10))
 (dbg-obj function-table)
 
 (def-obj override-functions
   (["square"
     (λ (begin
          (dbgl "inside debug square with " _)
          (add _ _)))]))
  
 (def-λ (virtual-dispatch object default-object function-name arg)
   (if (contains object function-name)
       ;call speciailised method
       ((get-prop object function-name) arg)
       ;call default
       ((get-prop default-object function-name) arg)))   

 (dbgl "dispatch:"
       (virtual-dispatch
        override-functions
        function-table
        "square" 10))

 ;how to handle things that modify or override some function
 ;for example, it usually takes

 (def mut 10)
 (set-var mut 20)
 (assert (mut = 20))

 (def-λ (mutation-test)
   ; mutating outside of closure scope
   (mut <- 30)
   (assert (mut = 30))
   ; shadow
   (def mut 50)
   (assert (mut = 50)))

 (mutation-test)
 (assert (mut = 30))

 (def-λ (mutation-test byval)
   (assert (mut = 30))
   (byval <- 40)
   (assert (mut = 30))
   (assert (byval = 40)))
 
 (mutation-test mut)

 
 (def obj2 (clone-obj function-table))
 (obj2.test <- "hello")
 (assert (obj2.test = "hello"))
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2) 

 ;elvis tests
 (def-obj elvis
   (["hamburger" (create-obj (["more-hamburgers" 42]))]))

 (assert
  (if (contains elvis "hamburger")
      (let ([burger elvis.hamburger])
        (if (contains burger "more-hamburgers")
            (eq burger.more-hamburgers 42)
            #f))
      #f))

 (assert
   (and (contains elvis "hamburger")
        (contains (get-prop elvis "hamburger") "more-hamburgers")
        (eq (get-prop (get-prop elvis "hamburger") "more-hamburgers")
                      42)))

 (assert (not (contains elvis "salads")))
 (assert (not elvis?.salads))
 (assert (contains elvis "hamburger"))
 (assert elvis?.hamburger)
 (assert elvis?.hamburger?.more-hamburgers)
 (assert (not elvis?.hamburger?.salads))
 (assert (not elvis?.salads?.moar-salads))
 (assert (elvis?.hamburger?.more-hamburgers = 42))
 (assert (elvis.hamburger.more-hamburgers = 42))

 (def players (vals (get-players))) 
 
 (def-λ (rock-paper-scissors)
   (let
     ([p1 (nth players 0)]
      [p2 (nth players 1)]
      [f (λ (clientid)
           (flow clientid "Choose a move!"
                 (["rock" "Play the rock!"]
                  ["paper" "Play the paper!"]
                  ["scissors" "Play the scissors!"])))]
      [r1 (f p1.clientid)]
      [r2 (f p2.clientid)])
     (cond
       [(r1 = r2) "you tie!"]
       [(or
         (and (r1 = "rock")     (r2 = "scissors"))
         (and (r1 = "scissors") (r2 = "paper"))
         (and (r1 = "paper")    (r2 = "rock")))
         "player 1 wins!"]
       [else "player 2 wins!"])))

; (dbgl (rock-paper-scissors))
 
; test closures and let scoping
 (def-λ (test-closure n)
   (let ([n2 7]
         [f (λ (x y)
              (begin
                (def ret (add n n2 x y))
                ; mutation inside closure
                (n2 += 1)
                (return ret)))])
     (return f)))

 (def test-closure-f (test-closure 37))
 (assert (eq (test-closure-f 10 20) 74))
 (assert (eq (test-closure-f 10 20) 75))

 (def test-list (arr (create-obj (["A" 1]["B" (create-obj (["A" 1]["B" 2]))]))
                 (create-obj (["A" 2]["B" 4]))))

;test break and indexed for loops
(for (item index test-list)
   #:break (index = 1)
   (dbg "item["index"]=")
   (dbg-obj item)
   (dbgl "")) 

;property nesting accessors and operators
(def-obj props-obj
   (["n" 10]
    ["prop1" (create-obj
             (["prop2" 42]))]))

(props-obj.n += 10)
(assert (props-obj.n = 20))
(assert (props-obj.prop1.prop2 = 42))
(props-obj.prop1.prop2 <- 58)
(assert (props-obj.prop1.prop2 = 58))
(props-obj.prop1.prop2 ++)
(assert (props-obj.prop1.prop2 = 59))
(props-obj.prop1.prop4 <- (create-obj (["final" (create-obj (["n" 10]))])))
(assert (props-obj.prop1.prop4.final.n = 10))
(props-obj.prop1.prop4.final.n --)
(assert (props-obj.prop1.prop4.final.n = 9))


;; ; runtime type tests


(assert (is-obj? props-obj))
(assert (is-arr? (arr)))



;U and Y combinators
(def-λ (U f) (f f))


((U (λ (self)
      (λ (num)
        (if (num = 0)
            (dbgl "finished!")
            (begin
              (dbgl "counting down " num)
              ((self self) (sub num 1)))))))
 10)

(def-λ (mk-counter f num)
  (if (num = 0)
      (dbgl "finished!")
      (begin
        (dbgl "counting down " num)
        (f (sub num 1)))))
                  
(def-λ (Y f)
  ((λ (g) (g g))
   (λ (g)
     (f (λ (x) ((g g) x))))))

((Y mk-counter) 10)

; funcjets
;; (def-λ (create-restorable-function init)
;;   (create-obj
;;    (["prev" (list)]
;;     ["app" init])))


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


 ;; lists
 (assert (is-list? (list)))
 (assert (not (is-list? 10)))
 (assert (not (is-arr? (list 20 20))))
 (def empty (list))

 (def test1 (list 10))
 
 (dbgl "test1 " (head test1))
 
 (def test2 (list 20 40))


 
 (dbgl "test2 " (head (tail test2)))

 (def-λ (test-rec lst)
   (if (eq lst #f)
       (dbgl "end of list")
       (begin
         (dbgl "item : " (head lst))
         (test-rec (tail lst)))))

 (test-rec test2)



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


