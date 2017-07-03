#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))

(define-syntax (obj-exists-in-loc stx)
  (syntax-parse stx
    [(_ loc matcher)
     #'(s-begin
         (def found 0)
         (def index 0)
         (def objs (get-objs loc))
         (s-while (s-and (s-not found) (ne index (list-len objs)))
           (def obj (nth index objs))
           (def index (add 1 index))
           (dbgl "index is " index)
           (obj-match obj
                      matcher
                      (def found 1))
           (dbgl "loop"))
         (s-return found))
        ]))
    
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
   (def-list camel-dice "green" "blue" "orange" "yellow" "white")
   (set-global "camel-dice" camel-dice)
   (def-obj temp (["type" "trap"]))
   (move-obj temp "track10")
   ;each player takes a turn where they can possibly:
   ; - take a pyramid card and cause a camel to move
   ; - take a leg betting card
   ; - place a trap
   ; - use a race betting card to bet on a winner or loser
   ; when no more camels can move, the leg is over and money
   ; is adjusted, then the camels are able to move again and the process
   ; continues
   (def camel-keys (keys camels))
   (def players (vals (get-players)))
   (s-while (get-global "race-on")
     (foreach (p players)
       (def client (get-prop p "clientid"))
       (def remaining-pyramid-cards (get-objs (get-loc "pyramid-stack")))
       (s-when (get-global "race-on")
         (s-unless (eq 0 (list-len remaining-pyramid-cards))
           (s-begin
            (def-req msg "choose an action")
            ; a pyramid cards gets those camels moving
            (add-req-action msg "pyramid-card" "take a pyramid card")
            ; if the player hasn't used their trap then add two actions for it
            ; one for the oasis and the other the mirage
            (s-when (contains p "trap")
              (add-req-action msg "trap-mirage" "place mirage trap")
              (add-req-action msg "trap-oasis" "place oasis trap"))
            
            ; add the top value card for each leg betting colour, if any are left
            (foreach (c colours)
              (def bet-loc (get-loc (add c "-bets")))
              (def cards (get-prop bet-loc "leg-cards"))              
              (s-unless (eq 0 (list-len cards))
                (add-req-action msg
                                (add c "-leg")
                                (add "bet on " c " to win the leg"))))
            
            (def resp (suspend msg client))
            (s-cond            

             [(eq resp "pyramid-card")
              (s-begin
               (cut)
               (dbgl "player chose a pyramid card")
               (call (play-pyramid-card p)))]

             [(contains resp "trap")
              (s-begin
               ;further suspend to chose the location
               (def is-oasis? (contains resp "oasis"))
               (call (play-trap p is-oasis?)))]

             
             [(contains resp "-leg")
              (s-begin
               (def colour (substring resp 0 (index-of resp "-")))
               (dbgl "player chose to bet on " colour " for this leg"))]
             
             [else (dbgl "player chose something else : " resp)])

            (s-when (eq 0 (list-len (get-objs (get-loc "pyramid-stack"))))
              (dbgl "leg over!")
              (def-list camel-dice "green" "blue" "orange" "yellow" "white")
              (set-global "camel-dice" camel-dice)
              (foreach (p2 players)
                (foreach (o (get-objs (get-prop p2 "clientid")))
                  (move-obj o "pyramid-stack"))))
                       

           ))
           ))
  ;   )
   

     )

   ; race over
   (foreach (c (vals camels))
     (dbg "camel " (get-prop c "colour") " is at location " (get-loc c))
     (dbgl ""))) 

 (def (play-trap player is-oasis?)
   (def-req trap-msg
     (s-if is-oasis?
           "where do you wish to place your oasis?"
           "where do you wish to place your mirage?"))
   (call(build-trap-location-actions trap-msg is-oasis?))
   (def trap-resp (suspend trap-msg client))
   (cut)
   (def loc (substring trap-resp 0 (index-of trap-resp "-")))
   (dbgl "player chose to place at " loc )
   ; move trap!
   (def trap (get-prop p "trap"))
   (set-prop trap "trap-type" (s-if is-oasis? "oasis" "mirage"))
   (move-obj trap (get-loc loc))
   (del-prop p "trap"))

 
 (def (build-trap-location-actions msg oasis?)
   ;second stage of trap action.
   ;traps can be placed in a location if:
   ;* no camels are there
   ;* a trap is not there
   ;* a trap is not either side
   (def track-index 1)
   (s-while (lt track-index 17)
     (def loc (get-loc (add "track" track-index)))
     (def next (get-loc (call (get-sibling loc "next"))))
     (def prev (get-loc (call (get-sibling loc "prev"))))
     (def cam (get-prop loc "camel-stack"))
     (s-when
      (s-and
       (eq 0 (list-len (get-prop loc "camel-stack")))
       (s-not (obj-exists-in-loc loc  ([(k v) ("type" "trap")])))
       (s-not (obj-exists-in-loc next ([(k v) ("type" "trap")])))
       (s-not (obj-exists-in-loc prev ([(k v) ("type" "trap")]))))
      (s-if oasis?
       (add-req-action
        msg
        (add "track" track-index "-oasis")
        ; it's midnight at the oasis, and all the camels are dead!
        (add "place an oasis at track " track-index))
       (add-req-action
        msg
        (add "track" track-index "-mirage")
        (add "place a mirage at track " track-index))))
     (def track-index (add 1 track-index))))
                       
 (def (play-pyramid-card player)
   ;pyramid cards aren't in stacks, just grab the first one
   ;from the location and move to the player location
   (def card (nth 0 (get-objs (get-loc "pyramid-stack"))))
   (set-prop card "owner" client)
   (move-obj card client)
   ;now get those camels moving!
   (def i (rndi (list-len (get-global "camel-dice"))))
   (def camel-dice (get-global "camel-dice"))
   (def new-list (remove-list camel-dice (nth i camel-dice)))
   (set-global "camel-dice" new-list)               
   (call
    (move-camel
     (get-prop camels (nth i camel-keys)) (rndi 1 4))))

 (def (get-sibling location key)
   (def sibs (get-siblings location))
   (def i 0)
   (s-while (eq 0 (contains (nth i sibs) key))
            (def i (add 1 i)))
   (s-return (nth i sibs)))
 
 ;; (def (get-track-offset current-track key amount)
 ;;   ; move forward or backwards via key locref amount times
 ;;   (s-if
 ;;    (eq amount 0)
 ;;    (s-return current-track)
 ;;    (s-begin
 ;;     (def target-loc (get-loc (call (get-sibling current-track key))))
 ;;     (call (get-track-offset target-loc key (sub amount 1))))))
 
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
   (s-while (lt cnt amount)
     (def sib (call (get-sibling target-loc "next")))
     (s-when (eq "finish" (get-prop sib "next"))
       (set-global "race-on" 0))
     (def target-loc (get-loc sib))
     (def cnt (add 1 cnt)))

   ; now we have the target location.
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
   (s-while (lt track-no 17)
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
   (link-location "track1" "track16" (["prev" "prev"]))

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
                  ["amount" a]
                  ["owner" ""])))
      (move-obj card bet)
      (append-list leg-cards card))
     (set-prop bet "leg-cards" leg-cards)
     ;one pyramid card for each camel
     (def cards (get-objs pyramid-stack))
     (move-obj (create-obj (["type" "pyramid-card"]
                            ["colour" c]
                            ["owner" ""])) pyramid-stack)
     ))
   
 (def (setup-players)
   ; get player list
   (def players (vals (get-players)))   

   (foreach (player players)
    (def client (get-prop player "clientid"))
    ; create location for player
    (ignore (create-location client "root"))
    ; each player gets 8 egyptian pounds
    ; a set of camel cards and a trap
    (def-list finish-cards)
    (foreach (c colours)
      ;one finish betting card for each colour camel!
      (append-list finish-cards
        (create-obj (["colour" c]
                     ["type" "camel-bet"]
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


