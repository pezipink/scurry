#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require syntax/parse/define)
(require threading)
(provide (all-defined-out))

(define-syntax-parser create-map-location
  [(_ name connections)
   #'(create-obj
      (["name" name]
       ["heroes" (list)]
       ["resources" 0]
       ["connections" connections]
       ["equipment" (list)]
       ["prey" 0]))])

(define-syntax-parser create-room
  [(_ name description cost supply-cost capacity avail action others ...)
   #'(create-obj
      (["name" name]
       ["desc" description]
       ["type" "room"]
       ["capacity" capacity]
       ["state" "unbuilt"]
       ["workers" 0]
       ["cost" cost]         ; material build cost
       ["supply-cost" supply-cost]
       ["avail?"
        (λ (this state)
          (and
           (eq this.state "built")
;           (gte this.workers this.capacity)
           (eq this.used? #f)
           (avail this state)))]
       ["action"
        (λ (this state)
          (this.used? <- #t)
          (action this state))]
       ["used?" #f]
       others ...
       )) ])

(define-syntax-parser create-equipment
  [(_ name description cost avail action others ...)
   #'(create-obj
      (["name" name]
       ["desc" description]
       ["type" "equipment"]
       ["state" "broken"]
       ["cost" cost]         ; fix equipment cost
       ["avail?"
        (λ (this state)
          (and
           (eq this.state "fixed")
           (eq this.used? #f)
           (avail this state)))]
       ["action"
        (λ (this state)
          (this.used? <- #t)
          (action this state))]
       ["used?" #f]
       others ...
       ))])

(define-syntax-parser create-hero
  [(_ location strength)
   #'(create-obj
      (["strength" strength]
       ["moved?" #f]
       ["location" location]))])

(define-syntax-parser create-leader
  [(_ name age starting-locations resources equipment)
   #'(create-obj
      (["name" name]
       ["age" age]
       ["starting-locations" starting-locations]
       ["resources" resources]
       ["equipment" equipment]))])

; there are a few types in outlive
; 1) the global play state. this includes
;    * global resources
;    * the turn counter
;    * the player states
;
; 2) the player state, this includes
;    * shelter data (see below)
;    * player's phase (will allow concurrent night phase later)
;    * available stuff to use (rooms, equip)
;    * hero data list (see below)
;    * current hero action data such as
;      - used actions
;      - last action result eg, search tile result,
;        resource collected, etc (whatever is needed for
;        certain item and room activation criteria)
;
; 3) shelter data 
;    * resources and materials
;    * buildings, built and unbuilt 
;    * equipment, broken and fixed
;    * radiactivity tracer
;
; 4) hero data
;    * location
;    * moved?  (heroes move once per turn)
;    * strength (total actions and pressure calcs)
;
; 5) equipment and rooms
;    * share some common properties 
;      - cost : repair or build cost in mats
;      - avail : function that determines if this item can
;                be used in the current phase
;      - action : function that returns a flow triple with
;                 the item action and chained result.
;
;    both these functions will be passed everything they 
;    need to determine their results: player state and room/item(self).  
;    a self reference is required in some cases where the thing needs
;    its own state. eg
;



(define-syntax (outlive-data stx)
  #'(begin
       ;global
      (def phase-dawn  "phase-dawn")  ;restock
      (def phase-day   "phase-day")   ;event processing and day stuff
      (def phase-night "phase-night") ;more events and night stuff
      
      ;foreach hero foreach player
      (def phase-day-2   "phase-day-2") ;move/scavenge/pressure/etc

      ;foreach player
      (def phase-night-1 "phase-night-1") ;overcome events
      (def phase-night-2 "phase-night-2") ;feed survivors
      (def phase-night-3 "phase-night-3") ;manage radioactivity
      (def phase-night-4 "phase-night-4") ;recurit survivors
      (def phase-night-5 "phase-night-5") ;build and use rooms
      (def phase-night-6 "phase-night-6") ;fix equipment
      (def phase-night-7 "phase-night-7") ;shelter upkeep
      
      (def leaders
        (list
         (create-leader "Wilson Fyre - Hunter" 44
           (list "blackwood" "silent-peak" "cargo-ship" "mine") (list "meat" "meat")   "shotgun")
         (create-leader "Erin McCarthy - Dowser" 17
           (list "dam" "blackwood" "silent-peak" "cargo-ship")  (list "water" "water") "jerrycan")
         (create-leader "Lily-Rose Wely - Geek" 20
           (list "blackwood" "silent-peak" "fair" "mine") (list "microchips" "microchips" "microchips") "backpack")
         (create-leader "Jacob Rowlett - Miner" 66
           (list "dam" "silent-peak" "cargo-ship" "fair") (list "metal" "metal" "microchips") "pickaxe")
         (create-leader "Swifty Bingham - Engineer" 19
           (list "dam" "military-base" "cargo-ship" "fair") (list "ammo" "microchips" "microchips") "thermal-sensor")
         (create-leader "Grayson Pigott - Brawler" 38
           (list "forest" "fair" "military-base" "fair" "mine") (list "ammo" "ammo" "microchips") "baseball-bat")
         (create-leader "Liza Valentine - Soldier" 25
           (list "military-base" "blackwood" "silent-peak" "cargo-ship") (list "ammo" "ammo" "ammo" ) "ammunitions-kit")
         (create-leader "Solen Livrich - Prowler" 32
           (list "forest" "blackwood" "cargo-ship" "mine") (list "canned-goods" "canned-goods") "grappling-hook")
         (create-leader "Kooper Froste - Lumberjack" 51
           (list "forest" "military-base" "blackwood" "silent-peak") (list "ammo" "wood" "wood") "axe")
         (create-leader "Mary Koolpepper - Police Officer" 23
           (list "forest" "blackwood" "cargo-ship" "mine") (list "blackwood-tile" "blackwood-tile" "silent-peak-tile") "flashlight")))
         
      (def-λ (create-city-tiles)
        (list "holy-water" "survival-ration" "plank" "heap-of-metal" "household-appliance" "ammunition" "ammunition"
              "seaweed-pills" "empty-cupboard" "empty-cupboard"))
      
      (def-λ (create-airlock)
        ; the airlock is special and is never "activated"
        ; and its supply cost is calcuated elsewhere
        (create-room "airlock" "the airlock" 0 0 7
                       (λ (this state) (return #f))
                       (λ (this state) (dbgl "!"))))
      
      (def-λ (create-starting-rooms)
        (list
          (create-room
           "draw-equip-repair"
           "draw one equipment tile and repair for 1 materials less"
           3 2 3
           (λ (this state) (eq state.phase phase-night-6))
           (λ (this state)
             (begin
               (dbgl "draw-equip-reapir was selected")
               (~>
                state
                (get-fixing-actions (λ (return (sub 1 (list-len _.cost)))))
                (map (λ (equip)
                       (tuple
                        equip.name
                        (add "Fix : " equip.desc " for cost -1 material" )
                        (λ (begin
                             (dbgl "player chose " _)
                             (fix-equipment state equip (λ (return (sub 1 (list-len equip.cost))))))))))
                (flow-from-triple state.clientid "Fix an item (special")))))

          (create-room
           "room-construction"
           "build one room with 2 less material"
           3 2 3
           (λ (this state) (eq state.phase phase-night-5))
           (λ (this state)
             (begin
               (dbgl "room-construction was selected")
               (~>
                state
                (get-building-actions (λ  (sub 2 _.cost)))
                (map (λ (room)
                       (tuple
                        room.name
                        (add "Build : " room.desc " for cost " (sub 2 room.cost) )
                        (λ (begin
                             (dbgl "player chose " _)
                             (build-room state room (λ  (sub 2 room.cost))))))))
                (flow-from-triple state.clientid "Build a room (special")))))))

        ;todo: overcome event
      

      (def equipment-prototypes
        (list
         (create-equipment
          "axe"
          "an axe (+2 wood from forest)"
          (list "wood" "metal" "metal")
          ;have to collect one wood first
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "bow"
          "a bow (+1 food from forest)"
          ; applies when entering forest
          (list "wood" "wood" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "jerrycan"
          "a jerrycan (+1 wood from dam)"
          ; have to collect one water first
          (list "wood" "metal" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))
         
         (create-equipment
          "access-card"
          "an access card (access the dam without the microchip cost)"
          (list "wood" "microchips" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "ammunitions-kit"
          "ammunitions kit (+2 ammo from military base)"
          ;collect one first
          (list "wood" "wood" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "crowbar"
          "a crowbar (2 free microchips from the military base)"
          (list "metal" "metal" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "chainsaw"
          "a chainsaw (2 free wood when entering a city)"
          (list "microchips" "microchips" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "flashlight"
          "a flashlight (gain one survivor to any room when entering a city)"
          (list "microchips" "microchips" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "pickaxe"
          "a pickaxe (+2 metal from mine)"
          ;collect one first
          (list "wood" "wood" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))
         
         (create-equipment
          "purifier"
          "a water purifier (1 free water when entering the mine)"
          (list "metal" "microchips" "microchips")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "grappling-hook"
          "a grappling hook (allows you to access the next level of the cargo ship)"
          (list "wood" "metal" "microchips")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))
         
         (create-equipment
          "hacksaw"
          "a hacksaw (2 free metal when enterting the cargo ship)"
          (list "wood" "metal" "microchips")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "backpack"
          "a backpack (+2 microchips from the fair)"
          ;collect first
          (list "wood" "wood" "wood")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "metal-detector"
          "a metal detector (1 free canned food when entering the fair)"
          (list)
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "shotgun"
          "a shotgun (+1 hunting strength)"
          (list "metal" "wood" "wood")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "bear-trap"
          "a bear trap (+1 hunting strength)"
          (list "wood" "metal" "metal")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "baseball-bat"
          "a baseball bat (+1 pressure)"
          (list "wood" "wood" "wood")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "battle-gear"
          "a set of battle gear (-2 pressure)"
          (list "wood" "microchips" "microchips")
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "exoskeleton"
          "an exoskeleton (one hero has no movement distance limitation)"
          (list)
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))

         (create-equipment
          "thermal-sensor"
          "a thermal sensor (you can select a prey from the stack when hunting)"
          (list)
          (λ (this state) (return #t))
          (λ (this state) (dbgl "!")))
         

         ))
                
))
