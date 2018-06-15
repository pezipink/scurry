
#lang racket

(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)
(require (for-syntax racket/syntax))


; mage knight has a hextile geometry with properties for each tile and possibly properties for the adjoining
; vertices.  these are easily reprenseted by locations and location references, using the following navigation encoding.
;  (using "columns" rahter than "rows")

;;
;
;     1
;  6     2         
;   tile 
;  5     3
;     4
;  or
;     a
;  f     b         
;   tile 
;  e     c
;     d
;  


;; additionally the game comes with several "super tiles" which pre-set tiles consisting of 7 of the above tiles, where all
;; all the outer verticies join to "unknown" tiles until the user discovers them and a new super tile is added.

(define-syntax-parser create-tile-data
  [(_ tile-name terrain special)
   #'{ "tile-name" tile-name
       "terrain" terrain
       "special" special }]
  [(_ tile-name terrain)
   #'{ "tile-name" tile-name
       "terrain" terrain }])
       
(define-syntax-parser create-super-tile-data
  [(_ tile-name tile-type
      ([z ...][a ...][b ...][c ...][d ...][e ...][f ...]))
   #'{ "super-tile-name" tile-name
       "super-tile-type" tile-type
       "z" (create-tile-data (tile-name + ":z")  z ...)
       "a" (create-tile-data (tile-name + ":a")  a ...)
       "b" (create-tile-data (tile-name + ":b")  b ...)
       "c" (create-tile-data (tile-name + ":c")  c ...)
       "d" (create-tile-data (tile-name + ":d")  d ...)
       "e" (create-tile-data (tile-name + ":e")  e ...)
       "f" (create-tile-data (tile-name + ":f")  f ...) }])

(define-syntax-parser create-super-tiles-data
  [(_ [name type vals] ...)
   #'(create-obj
      ([name (create-super-tile-data name type vals)] ...))])

(define-syntax-parser create-ac
  [(_ name colour ([type amount] ...) ([type2 amount2] ...))
   #'{ "name" name
       "colour" colour
       "basic"  (arr {"type" type "amount" amount} ... )
       "strong" (arr {"type" type "amount" amount} ... ) }]
    [(_ name colour ([type amount] ...) ([type2 amount2 special2] ...))
   #'{ "name" name
       "colour" colour
       "basic"  (arr {"type" type "amount" amount} ... )
       "strong" (arr {"type" type "amount" amount "special" special2} ... ) }])

(scurry
 (import core-lib)
 ;;terrain 
 (def-const-strings plains hills forest wasteland desert swamp lake mountain)
 ;;special tile
 (def-const-strings portal glade city village orcs draconum keep mage-tower monastery
   monster-den spawning-grounds city ancient-ruins crystal-mines)
 ;;super tile
 (def-const-strings countryside core)
 ;hero consts
 (def-const-strings arythea tovak norowas goldyx)
 ;card types
 (def-const-strings basic-action-card advanced-action-card spell-card artifact-card
   wound-card regular-unit-card elite-unit-card tactic-card)
 ;action types
 (def-const-strings attack block draw move influence heal special ranged-attack siege-attack)
 ;mana
 (def-const-strings red white blue green yellow black red-crystal white-crystal blue-crystal green-crystal)
 
 (def super-tiles
   (create-super-tiles-data
    ["super-tile-A" countryside
     ([plains portal][forest][plains][lake][lake][lake][plains])]
    ["super-tile-1" countryside
     ([forest glade][lake][plains village][plains][plains][forest][forest orcs])]
    ["super-tile-2" countryside
     ([hills][forest glade][plains village][plains][hills crystal-mines][plains]
      [hills orcs])]))

 (def-λ (tile->location tile)
   (let ([loc (create-location tile.tile-name)])
     (for (k (keys tile))
       (unless (k = "tile-name")
         ;copy props to the location
         (set-prop loc k (get-prop tile k))))
     (return loc)))
                  
 (def-λ (super-tile->location tile-name)
   (let ([tile (get-prop super-tiles tile-name)]
         [parent (create-location tile-name)]
         [centre (tile->location tile.z)]
         [a (tile->location tile.a)]
         [b (tile->location tile.b)]
         [c (tile->location tile.c)]
         [d (tile->location tile.d)]
         [e (tile->location tile.e)]
         [f (tile->location tile.f)]))
   (parent.super-tile-type <- tile.super-tile-type)
   (parent.super-tile-name <- tile.super-tile-name)
   ; setup relationships.  first assign children to the parent super location   
   (link-location-child parent centre "z" [])
   (link-location-child parent a "a" [])
   (link-location-child parent b "b" [])
   (link-location-child parent c "c" [])
   (link-location-child parent d "d" [])
   (link-location-child parent e "e" [])
   (link-location-child parent f "f" [])

   ; join the centre to its friends and the friends to the centre
   (link-location-sibling centre a "a" [])
   (link-location-sibling a centre "d" [])
   (link-location-sibling centre b "b" [])
   (link-location-sibling b centre "e" [])
   (link-location-sibling centre c "c" [])
   (link-location-sibling c centre "f" [])
   (link-location-sibling centre d "d" [])
   (link-location-sibling d centre "a" [])
   (link-location-sibling centre e "e" [])
   (link-location-sibling e centre "b" [])
   (link-location-sibling centre f "f" [])
   (link-location-sibling f centre "c" [])
   (return parent))

 (def basic-action-templates
   {
    "rage"          (create-ac "rage" red           ([attack 2][block 2]) ([attack 4]))
    "determination" (create-ac "determination" blue ([attack 2][block 2]) ([block 5]))
    "swiftness"     (create-ac "swiftness" white    ([move 2]  [block 1]) ([ranged-attack 3]))
    "march"         (create-ac "march" green        ([move 2])            ([move 4]))
    "stamina"       (create-ac "stamina" blue       ([move 2])            ([move 4]))
    "tranquility"   (create-ac "tranquility" green  ([heal 1]  [draw 1])  ([heal 2][draw 2]))
    "promise"       (create-ac "promise" white      ([influence 2])       ([influence 4]))
    "threaten"      (create-ac "threaten" red       ([influence 2])
                               ([influence 5 (λ (append-arr _.end-turn-events (λ (_.reputation --))))]))
   })
 
 
 
 (dbgl "creating tile location for super tile A")
 ;(def super-a (super-tile->location "super-tile-A"))

 (def-λ (generate-basic-deck)
   (map (arr "rage" "rage" "march" "march" "stamina" "stamina" "swiftness" "swiftness" "determination"
             "threaten" "promise" "tranquility" ) ;not complete
        (λ (clone-obj (get-prop basic-action-templates _)))))

 (def-λ (select-mage-knight player mk)
   ;setup player data.  
   (player.mage-knight <- mk)  ; mage knight name
   (player.deed-deck <- (generate-basic-deck))
   (player.discard-pile <- (arr))
   (player.armour <- 2)
   (player.units <- (arr))
   (player.hand-limit <- 5)
   (player.fame <- 0)
   (player.reputation <- 0)

   ;inventory (mana crystals)
   (player.blue-crystal <- 0)
   (player.red-crystal <- 0)
   (player.green-crystal <- 0)
   (player.white-crystal <- 0)
   
   (player.end-turn-events <- (arr))

   ;shuffle deck and deal starting hand
   ;(shuffle player.deed-deck);
   (player.hand <- (split-top player.hand-limit player.deed-deck))
   )
   
 (def-λ (mage-knight-select)
   (def mks (arr arythea tovak norowas goldyx))
   (for (p (vals (get-players)))
     (flow-end)
     (map-flow mks p.clientid "choose a mage-knight"
       (λ (tuple _ _
           (λ (begin           
                (select-mage-knight p _)
                (remove-arr mks _)))))))
   (flow-end))
       

 (def-λ (normal-turn p)
   (dbgl "entered normal turn")
   ; in a normal turn, the player can play cards in order
   ; to generate points for things like moving, fighting, etc.
   ; if they want to move it must happen before interactions

   ; all choice paths can be rolled back automatically via
   ; flowrouties, UNLESS new information is revealed or a
   ; dice is rolled.

   ;we deal with a clone of the cards on the stack
   ;so the flowroutines can undo everything.
   ;
   (def-λ (create-available-mana _)
     {"red-source" #f red-crystal 0
      "green-source" #f green-crystal 0
      "white-source" #f white-crystal 0
      "blue-source" #f blue-crystal 0 })

   (def-λ (create-turn-status _)
     (dbgl "un create turn status")
     {"source-uses" 1
      "action-taken" #f ; prevents further moving
      move 0 attack 0 block 0 heal 0 influence 0
      ranged-attack 0 siege-attack 0
      red white blue green})
   
   (def-λ (continue-turn cards-left cards-played mana-left turn-status)
     (dbgl "in continue turn")
     (dbgl "turn status:")
     (dbg-obj turn-status)
     
     (def-λ (continue card)
       (dbgl "in continue")
       (continue-turn
        (filter cards-left (λ (card != _)))
        (clone-append-arr cards-played card)
        mana-left
        turn-status        ))
     
     (def choices (arr))
     (def index 0)

     ;first collect all the possible actions that can be played from cards
     (for (c cards-left)
       (def used {red #f white #f blue #f green #f})
       (for (b c.basic)
           ; basic effects can always be used
           (begin
             (set-prop used b.type #t)
             (append-arr choices
               (tuple (->str index)
                      (add "play the " c.name " card for " b.amount " " b.type " points")
                      (λ (begin (prop+= turn-status b.type b.amount) (continue c)))))
             (index ++)))

       (for (b c.strong)
         ;strong  affects can only be used if the correct mana is available
         ;todo: add yellow and black mana depending on phase and source
         ;;(when (contains mana-left b.colour)
           ;; (append-arr choices
           ;;   (tuple (->str index)
           ;;          (add "play the " c.name " card for " b.amount " " b.type " points using one " c.colour " mana")
           ;;          (λ (begin (prop+= turn-status b.type b.amount) (continue c)))))
           ;;(index ++))
         )       
       

       (for (mt (keys used))
         ;cards can be played sideways for 1 of any of the core resources
         (unless (get-prop used mt)
           (append-arr choices
             (tuple (->str index)
                    (add "play the " c.name " card sideways for 1 " mt " point")
                    (λ (begin (prop+= turn-status mt 1) (continue c))))))
         (index ++)))
     (when ((len choices) > 0)
       (fft choices p.clientid "player turn")))

   (dbgl "calling..")
   (continue-turn p.hand (arr) (create-available-mana #f) (create-turn-status #f)))


 (mage-knight-select)

 (for (p (vals (get-players)))
   (normal-turn p))
 (dbgl "end")
 'brk
 'brk
)
