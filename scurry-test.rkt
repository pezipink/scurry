#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
    

(scurry
 
 (import core-lib)
 
 ; list tests

 ; split-top removes n elements and creates
 ; a new list from them
 (def x (list 1 2 3 4 5 6 7 8))

 (def y (split-top 3 x))

 (assert (lists-same? (list 1 2 3 4 5) x)
         "expected [1 2 3 4 5] but got " x)

 (assert (lists-same? (list 6 7 8) y)
         "expected [6 7 8] but got " y)

 ; split-bottom removes n elements from the
 ; front, creating a new list from them
 (def z (split-bottom 2 x))
 (assert (lists-same? (list 3 4 5) x)
         "expected [3 4 5] but got " x)
 (assert (lists-same? (list 1 2) z)
         "expected [1 2] but got " z)
 
 ; remove-list removes a single element

 (remove-list x 4)
 (assert (lists-same? (list 3 5) x)
         "expected [3 5] but got " x)

 (global.hello <- "world")

 (dbgl "hello global " global.hello)
 
 (def-obj function-table
   (["square" (λ (add _ _))]
    ["cube" (λ (add _ _ _))]
    ["twice" (λ (f x) (f (f x)))]))

 (dbgl "10 squared :" (function-table.square 10))
 (dbgl "10 cubed :" (function-table.cube 10))
 (dbgl "10 squared squared :"
       (function-table.twice function-table.square 10))

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

 ;elvis test
 (def-obj elvis
   (["hamburger" (create-obj (["more-hamburgers" 42]))]))

 
(assert (not (contains elvis "salads")))
(assert (not elvis?.salads))
(assert (contains elvis "hamburger"))
(assert elvis?.hamburger)
(assert elvis?.hamburger?.more-hamburgers)
(assert (not elvis?.hamburger?.salads))
(assert (not elvis?.salads?.moar-salads))
(assert (elvis?.hamburger?.more-hamburgers = 42))
(assert (elvis.hamburger.more-hamburgers = 42))

 ;probably better than this haha
 (assert
  (if (contains elvis "hamburger")
      (let ([burger elvis.hamburger])
        (if (contains burger "more-hamburgers")
            (eq burger.more-hamburgers 42)
            #f))
      #f))

 ; or this :)
 (assert
   (and (contains elvis "hamburger")
        (contains (get-prop elvis "hamburger") "more-hamburgers")
        (eq (get-prop (get-prop elvis "hamburger") "more-hamburgers")
                      42)))


 ; all equivalent
 
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2) 

 
 (dbgl "ft:")
 (dbg-obj function-table) 
 (dbgl "obj2")
 (dbg-obj obj2)
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

 (def test-list (list (create-obj (["A" 1]["B" (create-obj (["A" 1]["B" 2]))]))
                 (create-obj (["A" 2]["B" 4]))))

;test break and indexed for loops
(for (item index test-list)
   #:break (item = 0)
   (dbgl "item["index"]="item)) 


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

 ; is- tests
 (assert (is-list? (list)))
 (assert (not (is-list? 10)))
 (assert (is-bool? #t))
 (assert (not  (is-bool? (list))))
 (assert (is-obj? props-obj))

 
(def-λ (U f) (f f))

((U (λ (self)
      (λ (num)
        (if (num = 0)
            (dbgl "finished!")
            (begin
              (dbgl "counting down " num)
              ((self self) (sub 1 num)))))))
 10)

(def-λ (mk-counter f num)
  (if (num = 0)
      (dbgl "finished!")
      (begin
        (dbgl "counting down " num)
        (f (sub 1 num)))))
                  
(def-λ (Y f)
  ((λ (g) (g g))
   (λ (g)
     (f (λ (x) ((g g) x))))))

((Y mk-counter) 10)



'brk
 'brk

    (dbgl "end")
    )


