#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(scurry
 (def (main)
   (def-req req "test")
   (add-req-action req "a" "test a")
   (add-req-action req "b" "test b")
   (suspend-dispatch req "clienta"
     (["a" (dbg "client replied a")]
      ["b" (dbg "client replied b")]))
   ;(def resp (suspend req "test request"))

   ))




;; (scurry
;;  (def (main)
;;    (def-list colours "green" "blue" "orange" "yellow" "white")
;;    (set-global "race-on" 1)
;;    (call (init-locations))
;;    (call (setup-players))
;;    ;create the camels
;;    (def-obj camels)
;;    (def stack (get-prop (get-loc "track1") "camel-stack"))
;;    (foreach (c colours)
;;      (def-obj camel (["type" "camel"]
;;                      ["colour" c]))
;;      (move-obj camel (get-loc "track1"))
;;      (append-list stack camel)
;;      ;assign the camel to a quick-lookup dict by colour
;;      (set-prop camels c camel))
;;    (set-prop (get-loc "track1") "camel-stack" stack)


;;    ;each player takes a turn where they can possibly:
;;    ; - take a pyramid card and cause a camel to move
;;    ; - take a leg betting card
;;    ; - place a trap
;;    ; - use a race betting card to bet on a winner or loser
;;    ; when no more camels can move, the leg is over and money
;;    ; is adjusted, then the camels are able to move again and the process
;;    ; continues

;;    (def players (vals (get-players)))
;; ;   (while (get-global "race-on")
;;      (foreach (p players)
;;        (def remaining-pyramid-cards (get-objs (get-loc "pyramid-stack")))
;;        (when (get-global "race-on")
;;          (s-unless (eq 0 (list-len remaining-pyramid-cards))
;;            (s-begin
;;             (def-req msg "choose an action")
;;             (add-req-action msg "pyramid-card" "take a pyramid card")
;;             (def res (suspend msg (get-prop p "clientid"))))
;;            ; prepare options for player
;;            ))
;;            )
;;  ;    )
   
;;    ; race the camels around randomly
;;    (def camel-keys (keys camels))
;;    (while (get-global "race-on")
;;      (def i (rndi (list-len camel-keys)))
;;      (def spaces (rndi 1 3))
;;      (call (move-camel
;;             (get-prop camels (nth i camel-keys))
;;             spaces)))

;;    ; race over
;;    (foreach (c (vals camels))
;;      (dbg "camel " (get-prop c "colour") " is at location " (get-loc c))
;;      (dbgl ""))
;;    ) 
 
;;  (def (move-camel camel amount)
;;    (dbg "move-camel " (get-prop camel "colour") " " amount "\n")
;;    ; camels are stored in stack objects (lists) within a location
;;    ; under the camel-stack key.  the camel should be located and
;;    ; the stack split into two stacks - since the camels on top
;;    ; of the target camel get moved with it.  the new stack gets
;;    ; appended to the camel stack of the target location.

;;    ; first, work out the target destination by navigating through the
;;    ; "next" sibling of the locations.
;;    (def current-loc (get-loc camel))
;;    (def target-loc (get-loc camel))
;;    (def cnt 0)
;;    (while (lt cnt amount)
;;      ;todo: replace this with a find macro/func of some kind
;;      (def sibs (get-siblings target-loc))
;;      (def cnt2 0) 
;;      (while (eq 0 (has-prop (nth cnt2 sibs) "next"))
;;        (def cnt2 (add 1 cnt2)))
;;      (def sib (nth cnt2 sibs))
;;      (s-when (eq "finish" (get-prop sib "next"))
;;        (set-global "race-on" 0))
;;      (def target-loc (get-loc sib))
;;      (def cnt (add 1 cnt)))

;;    ; now we have the target location.
;;    (dbg "current-loc is " current-loc "\n")
;;    (dbg "target-loc is " target-loc "\n")
;;    ; grab the camel stack from the current location and split it
;;    ; such that the current camel and the rest of the list forms the new list
;;    (def current-stack (get-prop current-loc "camel-stack"))
;;    (def target-stack (get-prop target-loc "camel-stack"))
;;    (def-list new-current-stack)
;;    (def found-camel 0)
;;    (foreach (c current-stack)
;;      (s-when (eq c camel)
;;        (def found-camel 1))     
;;      (s-if (eq 1 found-camel)
;;        (s-begin
;;         (append-list target-stack c)
;;         (dbg "moving " (get-prop c "colour") " to " target-loc "\n")
;;         (move-obj c target-loc))
;;        ;else 
;;        (append-list new-current-stack c)))

;;    ;assign new lists
;;    (set-prop current-loc "camel-stack" new-current-stack)
;;    (set-prop target-loc "camel-stack" target-stack))
 
;;  (def (init-locations)
;;    (def root-loc (create-location "root"))

;;    ;entire track location
;;    (ignore (create-location "track" root-loc))
;;    ;first segment
;;    (def track (create-location "track1" "track"))
;;    (set-prop track "camel-stack" '(createlist))
;;    (def track-no 2)
;;    (while (lt track-no 17)
;;      ;create and link the next 15 sections
;;      (def curr-track (add "track" track-no))
;;      (def prev-track (add "track" (sub 1 track-no)))
;;      (def track (create-location curr-track "track"))
;;      (link-location curr-track prev-track (["prev" "prev"]))
;;      (link-location prev-track curr-track (["next" "next"]))
;;      (set-prop track "camel-stack" '(createlist))
;;      (def track-no (add 1 track-no)))
;;    ;wrap around at end, mark as finish line!
;;    (link-location "track16" "track1" (["next" "finish"]))

;;    (def pyramid-stack (create-location "pyramid-stack" root-loc))
;;    (def pyramid (create-location "pyramid" root-loc))

;;    ;betting card locations for the camels
;;    ;camel / dice tents
;;    (def tents-loc (create-location "tents-loc" root-loc))
;;    (def betting-loc (create-location "betting-loc" root-loc))

;;    (def-list amounts 2 3 5)
;;    (foreach (c colours)
;;      (def tent (create-location (add c "-tent") tents-loc))
;;      ;each tent has a coloured die       
;;      (def bet (create-location (add c "-bets") "betting-loc"))
;;      ;each betting location has 3 betting cards, 5 3 and 2.
;;      (def-list leg-cards)     
;;      (foreach (a amounts)
;;       (def card (create-obj
;;                  (["colour" c]
;;                   ["type" "leg-bet"]
;;                   ["owner" ""])))
;;       (move-obj card bet)
;;       (append-list leg-cards card))
;;      (set-prop tent "leg-cards" leg-cards)
;;      ;one pyramid card for each camel
;;      (def cards (get-objs pyramid-stack))
;;      (move-obj (create-obj (["type" "pyramid-card"]
;;                             ["colour" c]
;;                             ["owner" ""])) pyramid-stack)

;; ;     (set-prop 
;;      ))
   
;;  (def (setup-players)
;;    ; get player list
;;    (def players (vals (get-players)))   
;;    (foreach (player players)
;;     ; each player gets 8 egyptian pounds
;;     ; a set of camel cards and a trap
;;     (def client (get-prop player "clientid"))
;;     (def-list finish-cards)
;;     (foreach (c colours)
;;       ;one finish betting card for each colour camel!
;;       (append-list finish-cards
;;         (create-obj (["colour" c]
;;                      ["type" "camel-card"]
;;                      ["owner" client]))))
;;     (set-props
;;      player
;;      (["egyptian pounds" 8]
;;       ["trap" (create-obj (["type" "trap"]
;;                            ["owner" client]))]
;;       ["camel-cards" finish-cards]))
    
;;     ;debug text
;;     (dbg "player " client " now as the following props\n")
;;     (foreach (k (keys player))
;;       (dbg k " -> " (get-prop player k) "\n"))))
     

;; ) 


