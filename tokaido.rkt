#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)
(scurry
 (import core-lib)
 ;core events (to be moved into core)
 (set-global "on-enter" (list)) ;(go, loc)

 (def-λ (enter-farm player)
   (prop+= player "coins" 3))
   
 (def-λ (enter-temple player location)
   ;player can donate up to three coins to the temple
   (extract ([(clientid coins) player])
     (def-λ (coins->temple n)
       (prop-= player "coins" n)
       (prop+= player "points" n)
       (prop+= location (add clientid "-coins") n))
     (when (gt coins 0)              
       (flow clientid "donate to the temple"
         ([#t           "1 coin"  (~ coins->temple 1)]
          [(gt coins 1) "2 coins" (~ coins->temple 2)]
          [(gt coins 2) "3 coins" (~ coins->temple 3)]
          [#t           "do not donate" '()]))
       (flow-end))))

 (~ append-global-list "on-enter"
  (λ (player loc)
    (extract-match
     ([(ltype : type) loc]
      [(ptype : role) player])
     [(eq _ "temple") (eq _ "Hirotada")
      (~ (get-prop player "enter-temple") player loc)]
     [(eq _ "temple") _
      (~ enter-temple player loc)]
     [(eq _ "farm") _ (~ enter-farm player)]
     )))

 (def-λ (setup-player player)
   ;each player has a location keyed by their name
  (def client (get-prop player "clientid"))
  (def player-loc (create-location client))
  ;the location has the various places where their cards are held
  ;todo: clean this stuff up wtih some nice macros
  ;todo todo: generate this with an editor!!
  (ignore
   (create-location (add client "-hot-springs") player-loc)
   (create-location (add client "-meals") player-loc))
  ;; (def panoramas (create-location "panoramas" player-loc))
  
  ;; (create-location "sea-panorama" player-loc)
  ;;  (create-location "paddy-panorama" player-loc)
  ;;  (create-location "mountain-panorama" player-loc)
   
  ;; (def souvenirs (create-location "souvenirs" player-loc))
  ;; (set-props souvenirs
  ;;            (["type1" '(createlist)]
  ;;             ["type2" '(createlist)]))
  
  (extract-match ([(role) player])
    [(eq _ "Hirotada")
     (begin
       (set-props player
         (["coins" 8]
          ["points" 0]
          ["enter-temple"
           (λ (player location)
             ;in addition to the normal temple donations,
             ;hirotada can also "dontate" a coin straight from the
             ;reserve, and score immediately for it.
             (extract ([(coins clientid) player])
               (flow clientid "donate to the temple (special ability)"
                 ([#t "1 coin from the reserve"
                   (begin
                     (prop+= player "points" n)
                     (prop+= location (add clientid "-coins") n))]
                  [#t "skip" '()]))
               (flow-end))
             (~ enter-temple player location)
          ;call normal func
          )]
       )))]))

 (def players (vals (get-players))) 
 (def temple (create-location "temple"))

 (set-props temple (["coins" 0]["type" "temple"]))

 (def i 0)
 (foreach (p players)
   (if (eq i 0)
    (begin
     (set-prop p "role" "Hirotada")
     (~ setup-player p)
     (++ i))
    (begin
     (set-props p (["coins" 10]["points" 0]["role" "neutral"]))
     ))
   (set-prop temple (add (get-prop p "clientid") "-coins") 0)
   (dbgl "player has " (get-prop p "coins"))
   (foreach (f (get-global "on-enter"))          
            (~ f p temple))
   (dbgl "player now has " (get-prop p "coins"))
   (dbgl "temple now has " (get-prop temple "coins"))
         
         )
 

 )


