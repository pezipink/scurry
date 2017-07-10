#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require threading)

(define-syntax (obj-exists-in-loc stx)
  (syntax-parse stx
    [(_ loc matcher)
     #'(s-begin
         (def found 0)
         (def index 0)
         (def objs (get-objs loc))
         (s-while (s-and (s-not found) (ne index (list-len objs)))
           (def obj (nth index objs))
           (++ index)
           (dbgl "index is " index)
           (obj-match obj
                      matcher
                      (++ found))
           (dbgl "loop"))
         (s-return found))
        ]))
    
(define-syntax (obj-not-exists-in-locs stx)
  (syntax-parse stx
    [(_ loc ... matcher)
     #'(s-and
        (s-not (obj-exists-in-loc loc matcher)) ...)]))

(define-syntax (list-zero stx)
  (syntax-parse stx
    [(_ lst) #'(eq 0 (list-len lst))]))

(scurry
 (def (main)
   (def-list colours "green" "blue" "orange" "yellow" "white")
   (set-global "race-on" 1)

   (call (init-locations))
   (call (setup-players))

   ;create the camels
   (def-obj camels) ; lookup dict

   (def-list stack)

   (foreach (c colours)
     (def-obj camel (["type" "camel"]
                     ["colour" c]))
     (def rnd-track (add "track" (rndi 1 4)))
     (move-obj camel (get-loc "track1"))
     (append-list stack camel)
     ;assign the camel to a quick-lookup dict by colour
     (set-prop camels c camel))
   (dbgl "stack is " stack)
   (~> (get-loc "track1")
       (set-prop "camel-stack" stack))

   (foreach-reverse
    (c stack)
    (call (move-camel c (rndi 0 3)))
       (dbgl "stack is now" stack)
    )
   
   (def-list camel-dice "green" "blue" "orange" "yellow" "white")
   (set-global "camel-dice" camel-dice)

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
              
       (def remaining-pyramid-cards
         (~> "pyramid-stack" get-loc get-objs))

       (s-when (get-global "race-on")
         (s-unless (list-zero remaining-pyramid-cards)
           (s-begin
            (def-flow msg "choose an action")
            (call (add-base-actions msg p))            
            (def resp (flow msg client))
            (dbgl "got response " resp)
            (s-cond            
             [(eq resp "pyramid-card")
              (s-begin
               (flow-end)
               (dbgl "player chose a pyramid card")
               (call (play-pyramid-card client)))]

             [(contains resp "trap")
              ;further suspend to chose the location
              (s-begin
               (dbgl "calling trap..")
               (call (play-trap p (contains resp "oasis"))))]
             
             [(contains resp "-leg")
              (s-begin
               (flow-end)
               (def colour (substring resp 0 (index-of resp "-")))
               (dbgl "player chose to bet on " colour " for this leg")
               (call (take-leg-card p colour))
               )]
             
             [else (dbgl "player chose something else : " resp)])

            ;test if leg is over
            (s-when (~> (get-loc "pyramid-stack") get-objs list-zero)
              (dbgl "leg over!")
              ;reset dice
              (def-list camel-dice "green" "blue" "orange" "yellow" "white")
              (def camel-positions (call (get-camel-positions)))
              (set-global "camel-dice" camel-dice)
              ; score points and move leg cards / traps back
                
              (foreach (p2 players)
                (foreach (o (~> (get-prop p2 "clientid") get-objs))
                  (s-cond
                   [(has-key-value o "type" "pyramid-card")
                    (s-begin
                     (dbgl "pyramid card!")
                     (move-obj o "pyramid-stack"))]
                   [else (dbgl "something else!!")]))))                 
            ))
         ))
     )

   ; race over
   (dbgl "children")
   (def camel-positions (call (get-camel-positions)))
   (foreach(c (get-children "track"))
           (dbgl c))
   (foreach (c (vals camels))
     (dbg "camel " (get-prop c "colour") " is at location " (get-loc c))
     (dbgl "")
     (dbgl "the winning stack is at location " (get-global "winning-loc"))
     )
   
   )

 (def (get-camel-positions)
   (def-list camel-positions)
   (foreach-reverse (t (~> "track" get-loc get-children))
    (foreach-reverse (c (get-prop (get-loc t) "camel-stack"))
     (append-list camel-positions c)))
   (dbgl "camel positions : " camel-positions)
   (s-return camel-positions))
 
 (def (add-base-actions flow player)
   ; a pyramid cards gets those camels moving
   (add-flow-action msg "pyramid-card" "take a pyramid card")
   ; if the player hasn't used their trap then add two actions for it
   ; one for the oasis and the other the mirage
   (s-when (contains p "trap")
           (add-flow-action msg "trap-mirage" "place mirage trap")
           (add-flow-action msg "trap-oasis" "place oasis trap"))
   
   ; add the top value card for each leg betting colour, if any are left
   (foreach (c colours)
            (s-unless
             (~>(add c "-bets")
                get-loc
                (get-prop "leg-cards")
                list-zero)
             (add-flow-action
              msg
              (add c "-leg")
              (add "bet on " c " to win the leg")))))
            
 (def (modify-pounds player amount)
   (~> (get-prop player "amount")
       (add amount)
       (set-prop player "amount" _)))
   
 (def (play-trap player is-oasis?)
   (def client (get-prop player "clientid"))
   (dbgl "in play-trp")
   (def-flow trap-msg
     (s-if is-oasis?
           "where do you wish to place your oasis?"
           "where do you wish to place your mirage?"))
   (call (build-trap-location-actions trap-msg is-oasis?))
   (dbgl "calling flow..")
   (def trap-resp (flow trap-msg client))
   (flow-end)
   (def loc (substring trap-resp 0 (index-of trap-resp "-")))
   (dbgl "player chose to place at " loc )
   ; move trap!
   (def trap (get-prop player "trap"))
   (set-prop trap "trap-type" (s-if is-oasis? "oasis" "mirage"))
   (move-obj trap (get-loc loc))
   (del-prop player "trap"))

 
 (def (build-trap-location-actions msg oasis?)
   ;second stage of trap action.
   ;traps can be placed in a location if:
   ;* no camels are there
   ;* a trap is not there
   ;* a trap is not either side
   (dbgl "in trap locations")
   (def track-index 1)
   (s-while (lt track-index 17)
     (def loc (get-loc (add "track" track-index)))
     (def next (get-loc (call (get-sibling loc "next"))))
     (def prev (get-loc (call (get-sibling loc "prev"))))
     (def cam (get-prop loc "camel-stack"))
     (s-when
      (s-and
       ; no camels
       (eq 0 (list-len (get-prop loc "camel-stack")))
       (list-zero (get-prop loc "camel-stack"))
       ; no traps
       (obj-not-exists-in-locs loc next prev
                                    ([(_ _) ("type" "trap")])))
      (dbgl "got this far")
      (s-if oasis?
       (add-flow-action
        msg
        (add "track" track-index "-oasis")
        ; it's midnight at the oasis, and all the camels are dead!
        (add "place an oasis at track " track-index))
       (add-flow-action
        msg
        (add "track" track-index "-mirage")
        (add "place a mirage at track " track-index))))
     (++ track-index)))
                       
 (def (take-leg-card player colour)
   (def client (get-prop player "clientid"))
   (def loc (get-loc (add colour "-bets")))
   (def card (call (pop-prop-list "leg-cards" loc)))
   (move-obj card (get-loc client))
   (dbgl " player " player " took leg betting card for " (get-prop card "amount")))

 
 (def (play-pyramid-card client)
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
   (s-while (s-not (contains (nth i sibs) key))
            (++ i))
   (s-return (nth i sibs)))
 
 (def (move-camel camel amount)
   (s-when
    (ne amount 0)
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
    (s-while
     (lt cnt amount)
     (def sib (call (get-sibling target-loc "next")))
     (s-when (eq "finish" (get-prop sib "next"))
             (set-global "race-on" 0))
     (def target-loc (get-loc sib))
     (++ cnt))

    ; now we have the target location.
    ; grab the camel stack from the current location and split it
    ; such that the current camel and the rest of the list forms the new list
    (def current-stack (get-prop current-loc "camel-stack"))
    (def target-stack (get-prop target-loc "camel-stack"))
    (def-list new-current-stack)
    (def found-camel 0)
    (foreach
     (c current-stack)
     (s-when (eq c camel)
             (++ found-camel))     
     (s-if
      (eq 1 found-camel)
      (s-begin
       (append-list target-stack c)
       (dbg "moving " (get-prop c "colour") " to " target-loc "\n")
       (move-obj c target-loc))
      ;else 
      (append-list new-current-stack c)))

    ;assign new lists
    (set-prop current-loc "camel-stack" new-current-stack)
    (set-prop target-loc "camel-stack" target-stack)
    ;if the game is over, store the winning track location
    ;to help scoring
    (set-global "winning-loc" target-loc)
    ))
 
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
     
   (def (pop-prop-list prop-key obj)
     (def temp-existing-list (get-prop obj prop-key))
     (dbgl "temp list is " temp-existing-list)
     (def-list temp-new-list)
     (def temp-counter 0)
     (s-while (lt temp-counter (sub 1 (list-len temp-existing-list)))
              (append-list temp-new-list (nth temp-counter temp-existing-list))
              (++ temp-counter))
     (set-prop obj prop-key temp-new-list)
     (dbgl "new peop list is " (get-prop obj prop-key))
     (s-return (nth temp-counter temp-existing-list)))
            
        

) 


