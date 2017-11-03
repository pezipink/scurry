#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)
(require (for-syntax syntax/parse))
(require (for-syntax racket/list))
;; [(can-buy? 0 avail) (to-description 0 avail)
;;            (begin
;;              (buy 0 avail)
;;              (aux avail))]
          
;; (define-syntax (replace-n stx)
;;   (syntax-parse stx
;;     [(_ n:int (exprs:expr ...))
     
     



(scurry
 

 (import core-lib)
 ;core events (to be moved into core)
 (set-global "on-enter" (list)) ;(go, loc)

 (def-λ (enter-farm player)
   (player.coins += 3))

 (def-λ (enter-hot-spring player)
   (def springs global.hot-springs)
   (when (gt (list-len springs) 0)
     (deal (global-obj) "hot-springs" player "hot-springs" 1)))

 (def-λ (enter-village player)
   ;the player views and can buy up to three souvenirs
   (extract ([(clientid) player]
             [(souvenirs) (global-obj)])
     
     (def-λ (can-buy? index avail)
       (and (lt index (list-len avail))
            (gt player.coins (get-prop (nth index avail) "cost"))))

     (def-λ (to-description index avail)
       (extract ([(cost name souvenir-type) (nth index avail)])
         (add "buy the " name " souvenir of type " souvenir-type " for " cost)))

     (def-λ (buy index avail)
       (def item (nth index avail))
       (player.coins -= item.cost)
       ;todo: move to appropriate player area
       (remove-list avail item))
     
     (def-λ (aux avail)
       (flow-end)
       (flow clientid "buy souvenirs"
         ([(can-buy? 0 avail) (to-description 0 avail)
           (begin
             (buy 0 avail)
             (aux avail))]
          [(can-buy? 1 avail) (to-description 1 avail)
           (begin
             (buy 1 avail)
             (aux avail))]
          [(can-buy? 2 avail) (to-description 2 avail)
           (begin
             (buy 2 avail)
             (aux avail))]  
          [#t
           "do not buy"
           '()])))

     ; cards not bought go back on the bottom of the pile
     ;and removed from the clients universe
     (dbgl "sovs before : " souvenirs)
     (def n (min 3 (list-len souvenirs)))            
     (def avail (split-top n souvenirs))
     ;move all three to the player area
     (for (s avail) (move-obj s clientid))
     ;there is a problem here - we always end up with all the
     ;list data instead of the stuff that wasn't bought.
     (aux avail)

     ; remove the remaining cards from the clients
     (for (s avail) (remove-uni s))
     (prepend-many avail souvenirs)
     'brk
     (dbgl "sovs after : " souvenirs)
     ;(set-global "souvenirs" left)
     ))

 (def-λ (enter-temple player location)
   ;player can donate up to three coins to the temple
   (extract ([(clientid coins) player])
     (def-λ (coins->temple n)
       (player.coins += n)
       (player.points += n)
       (prop+= location (add clientid "-coins") n))
     (when (gt coins 0)
       (flow clientid "donate to the temple"
         ([#t           "1 coin"  (coins->temple 1)]
          [(gt coins 1) "2 coins" (coins->temple 2)]
          [(gt coins 2) "3 coins" (coins->temple 3)]
          [#t           "do not donate" '()]))
       (flow-end))))

 (append-global-list "on-enter"
  (λ (player loc)
    (extract-match
     ([(ltype : type) loc] [(ptype : role) player])
     [(eq _ "temple") (eq _ "Hirotada")
      (player.enter-temple player loc)]
     [(eq _ "temple") _
      (enter-temple player loc)]
     [(eq _ "farm") _
      (enter-farm player)]
     [(eq _ "hot-spring") _
      (enter-hot-spring player)]
     [(eq _ "village") _
      (enter-village player)])))

 (def-λ (setup-player player)
   ;each player has a location keyed by their name
  (def player-loc (create-location player.clientid))
  (move-obj player player-loc)
  ;the location has the various places where their cards are held
  ;todo: clean this stuff up wtih some nice macros
  ;todo todo: generate this with an editor!!
  (ignore
   (create-location (add player.clientid "-hot-springs") player-loc)
   (create-location (add player.clientid "-meals") player-loc))

  
  
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
          ["hot-springs" (list)]
          ["enter-temple"
           (λ (player location)
             ;in addition to the normal temple donations,
             ;hirotada can also "donate" a coin straight from the
             ;reserve, and score immediately for it.
             (extract ([(coins clientid) player])
               (flow clientid "donate to the temple (special ability)"
                 ([#t "1 coin from the reserve"
                   (begin
                     (player.points += 1)
                     (prop+= location (add clientid "-coins") 1))]
                  [#t "skip" '()]))
               (flow-end))
             (enter-temple player location)
          ;call normal func
          )])))]))
 
 (def players (vals (get-players))) 
 (def temple (create-location "temple"))
 (def spring (create-location "hot-spring1"))
 (def village (create-location "village1"))
  
 (set-props temple (["coins" 0]["type" "temple"]))
 (set-props spring (["type" "hot-spring"]))
 (set-props village (["type" "village"]))
 
 (def-λ (create-spring value)
    (create-obj (["type" "hot-spring"]
                 ["value" value])))

 (def souvs
   (list
    (create-obj (["type" "souvenir"]
                 ["souvenir-type" "A"]
                 ["name" "juan1"]
                 ["cost" 1]))
    (create-obj (["type" "souvenir"]
                 ["souvenir-type" "B"]
                 ["name" "juan"]
                 ["cost" 2]))
    (create-obj (["type" "souvenir"]
                 ["souvenir-type" "A"]
                 ["name" "juan3"]
                 ["cost" 3]))
    (create-obj (["type" "souvenir"]
                 ["souvenir-type" "A"]
                 ["name" "juan4"]
                 ["cost" 4]))))

 (set-global "souvenirs" souvs)
 (def hot-springs (list))
 (def i 0)
 (while (lt i 6)
   (++ i)
   (append-list hot-springs (create-spring 6))
   (append-list hot-springs (create-spring 12)))


 (set-global "hot-springs" hot-springs)

 
 (def i 0)
 (for (p players)
   (if (eq i 0)
    (begin
     (set-prop p "role" "Hirotada")
     (setup-player p)
     (++ i))
    (begin
      (set-props p (["coins" 10]
                    ["points" 0]
                    ["hot-springs" (list)]
                    ["role" "neutral"]))
      (def player-loc (create-location p.clientid))
      (move-obj p player-loc)

     ))
   (set-prop temple (add p.clientid "-coins") 0)
   ;; (dbgl "player has " (get-prop p "coins"))
   ;; (foreach (f (get-global "on-enter"))          
   ;;          (~ f p temple))
   ;; (dbgl "player now has " (get-prop p "coins"))
   ;; (dbgl "temple now has " (get-prop temple "coins"))

   ;; (dbgl "player has " (get-prop p "hot-springs"))
   ;; (foreach (f (get-global "on-enter"))          
   ;;          (~ f p spring))
   ;; (dbgl "player has " (get-prop p "hot-springs"))          

 (for (f (get-global "on-enter"))          
             (f p village)))
 (dbgl "the end")
 'brk
 )


