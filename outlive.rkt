#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require "outlive-data.rkt")
(require threading)
(require syntax/parse/define)

(scurry
 (import core-lib)

 (def-λ (expand-cost-type cost-type)
   (case cost-type
     ["resources" (list "wood" "metal" "microchips" "ammo")]
     ["materials" (list "wood" "metal" "microchips")]
     ["supplies"  (list "meat" "water" "canned-gooods")]
     [else (list cost-type)]))
      
 (def-λ (pay-survivors player-state amount)
   (def shelter player-state.shelter)
   ; survivors can be taken from rooms with workers or the airlock
   (def paid 0)
   (while (lt paid amount)
     (def rooms
       (~>
        (list player-state.airlock)
        (append-many (filter player-state.stuff (λ (_.type = "room"))))
        (filter (λ (_.workers > 0)))))
     (if (eq (list-len rooms) 0)
         (begin
           (dbgl "no more survivors!")
           (def paid amount)) ; need <- or set here!
         (begin
           (~>
            (map rooms (λ (room)
                (tuple
                 room.name
                 (add "sacrifice a survivor from room \""
                      room.name
                      "\" (" room.workers " workers)")
                 (λ (room.workers -= 1)))))
            (flow-from-triple
             player-state.clientid
             "Sacrifice a survivor"))))
     (++ paid)))          

 (def-λ (pay-cost-specific player-state cost-types amount)
   ; if the cost types length is the same as the amount, we can just take
   ; the resources directly from the shelter, otherwise the client
   ; can pick n combination of them as relevant.
   (dbgl "in pay-cost-specific " cost-types " "  amount)
   (def shelter player-state.shelter)
   (if (eq (list-len cost-types)  amount)
       (begin
         (for (c cost-types)
           (prop-= shelter c 1)))
       (begin
         (def-λ (create-req type)
           (tuple type
             (add "pay 1 " type "(" (get-prop shelter type) " remaining)")
             (λ (prop-= shelter type 1))))
         (def paid 0)
         (while (lt paid amount)
           (~>
            cost-types
            (filter (λ (gt (get-prop shelter _) 0)))
            (map create-req)
            (flow-from-triple
             player-state.clientid
             (add "pay the cost (" paid "/" amount ")")))
           (flow-end)
           (++ paid)))))
        
 (def-λ (pay-cost player-state cost-type amount)   
   (def paid 0)
   (def shelter player-state.shelter)
   
   (def-λ (create-req type)
     (tuple type
       (add "pay 1 " type "(" (get-prop shelter type) " remaining)")
       (λ (prop-= shelter type 1))))

   (while (lt paid amount)
     (~>
      (expand-cost-type cost-type)
      (filter (λ (gt (get-prop shelter _) 0)))
      (map create-req)
      (flow-from-triple
        player-state.clientid
        (add "pay the cost (" paid "/" amount ")")))
     (flow-end)
     (++ paid)))

 (def-λ (shelter-has-enough-specific shelter cost-types amount)
   (dbgl "in shelter-has-enough-speicifc" amount)
   (def counts (create-obj))
   (for (c cost-types)
     (unless (contains counts c)
       (set-prop counts c 0))
     (prop+= counts c 1))
   (def total
     (fold (keys counts) 0
           (λ (acc c)
              (add acc (min (get-prop counts c) (get-prop shelter c))))))
   (gte total amount))
      
 (def-λ (shelter-has-enough shelter cost-type amount)
   (dbgl "in shelter-has-enough " amount)
   (case cost-type
     ["materials"
      (gte (add shelter.wood shelter.metal shelter.microchips) amount)]
     ["supplies"
      (gte (add shelter.meat shelter.canned-goods shelter.water) amount)]
     [else (gte (get-prop shelter cost-type) amount)]))

 (def-λ (fix-equipment player-state equip cost-modifier)
   (dbgl "in fix-equipment")
   (pay-cost-specific player-state equip.cost (cost-modifier equip))
   (equip.state <- "fixed")   
   (flow-end))
  
 (def-λ (build-room player-state room cost-modifier)
   (dbgl "in build-room")
   (pay-cost player-state "materials" (cost-modifier room))
   (room.state <- "built")   
   (flow-end))
 
 (def-λ (get-building-actions player-state cost-modifier)
   (dbgl "in get building actions")
   (def shelter player-state.shelter)
   (~>
    shelter.stuff
    (filter (λ (and (_.type = "room") (_.state = "unbuilt"))))
    (filter (λ (shelter-has-enough
                shelter "materials" (cost-modifier _))))))

 (def-λ (get-fixing-actions player-state cost-modifier)
   (dbgl "in get fixing actions")
   (def shelter player-state.shelter)
   (~>
    shelter.stuff
    (filter (λ (and (_.type = "equipment") (_.state = "broken"))))
    (filter (λ (shelter-has-enough-specific
                shelter _.cost (cost-modifier _))))))

  (def-λ (get-stuff-activations player-state)
   (dbgl "in get stuff activations")
   (def shelter player-state.shelter)
   (~>
    shelter.stuff
    (filter (λ (return (_.avail? _ player-state))))))

  (def-λ (try-get-from-resources type amount)
    (def res global.resources)
    (def actual (min amount (get-prop res type)))
    (prop-= res type actual)
    (return actual))
    
 (import outlive-data)

 (def players (vals (get-players)))
 
 (global.turn <- 0)
 (global.resources <-
   (create-obj
    (["wood" 30]
     ["metal" 30]
     ["microchips" 30]
     ["water" 30]
     ["meat" 30]
     ["canned-goods" 30]
     ["ammo" 30]
     ["survivors" 100])))
 (global.phase <- phase-dawn)
 (global.events <- (list))
 (global.locations <-
   (create-obj
    (["dam"           (create-map-location "dam" (list "forest" "military-base"))]
     ["forest"        (create-map-location "forest" (list "dam" "blackwood"))]
     ["blackwood"     (create-map-location "blackwood" (list "forest" "fair"))]
     ["fair"          (create-map-location "fair" (list "blackwood" "cargo-ship"))]
     ["cargo-ship"    (create-map-location "cargo-ship" (list "fair" "mine"))]
     ["mine"          (create-map-location "mine" (list "cargo-ship" "silent-peak"))]
     ["silent-peak"   (create-map-location "silent-peak" (list "mine" "military-base"))]
     ["military-base" (create-map-location "military-base" (list "silent-peak" "dam"))])))
 (global.equipment <- (list))
 
 ;each piece of equipment exists twice
 (for (e equipment-prototypes)
   (append-list global.equipment (clone-obj e))
   (append-list global.equipment (clone-obj e)))
 
 ; initial object setup
 (for (p players)
   (dbgl "setting up player " p.clientid)
   (def-obj shelter
     (["airlock" (create-airlock)]
      ["stuff" (create-starting-rooms)] ; todo: distribute advanced rooms

      ["ammo" 0]

      ;supplies
      ["water" 0]
      ["meat" 0 ]
      ["canned-goods" 0]
      
      ;materials
      ["microchips" 0]
      ["metal" 0]
      ["wood" 0]

      ["radioactivity" 0]
      ["leader" ""] ;todo

      ;killed prey
      ["prey" (create-obj
               (["3" 0]["4" 0]
                ["5" 0]["6" 0]
                ["7" 0]))]
      ))

   ;(dbg-obj shelter)
   
   (def-obj action-data
     (["used-actions" 0]
      ["last-result" ""]))

   (set-props p
     (["shelter" shelter]
      ["phase" phase-day-2]
      ["heroes"
        (list
         (create-hero "" 3)
         (create-hero "" 3)
         (create-hero "" 4)
         (create-hero "" 5))]
      ["action-data" action-data]))

   (def-λ (leader->desc leader)
     (def desc leader.name)
     (return desc))

   (def-λ (choose-leader leader)
     (shelter.leader <- leader)
     (for (r leader.resources)
       (case r
         ["blackwood-tile" (dbgl "blackwood tile")]
         ["silent-peak-tile" (dbgl "silent peak tile")]
         [else (prop+= shelter r 1)]))

     (def equip (first global.equipment (λ (_.name = leader.equipment))))
     (if (equip = #f)
         (begin (dbgl "could not find equipment " leader.equipment)
                'brk)
         (begin
           (remove-list global.equipment equip)
           (append-list shelter.stuff equip)))
     
     (def locations (clone-list leader.starting-locations))
     (for (h p.heroes)
       (flow-end)
       (def title (add "Select a starting location for your strength " h.strength " hero."))
       (~>
        locations
        (map (λ (loc)
               (tuple loc loc
                 (λ (begin
                      (h.location <- _)
                      (remove-list locations _))))))
        (flow-from-triple p.clientid title))))
     
   (~>
    leaders
    (map (λ (leader)
           (tuple leader.name
                  (leader->desc leader)
                  (λ (choose-leader leader)))))
    (flow-from-triple p.clientid "Choose a leader"))
   
   (def starting-loc "home") ;todo: leader dependent
   ;todo: pick a building to build for free
   ; and move starting survivors into the airlock
   )
    

 ; core loop functions
 
 (def-λ (process-dawn)
   ;replenish board resources
   (dbgl "process-dawn"))

 (def-λ (process-day)
   ; new events, hero movement, scavenging and pressure
   (dbgl "process-day")

   (dbgl "processing events...")

   (for (p players)
     (dbgl "processing player " p.clientid)
     ;todo: this processing will be as the player determines it
     ;and not just a loop
     (p.phase <- phase-day-2)
     (for (h p.heroes)      
      (dbgl "\tprocessing hero " h.strength " at location " h.location)))
   )
 
 (def-λ (process-night)
   ;event resolution, feeding, radioactivity, building, fixing
   ; todo: this can be done concurrently for each player   
   (dbgl "process-night")
   (for (p players)
     (def shelter p.shelter)
     (dbg-obj shelter)
     (p.phase <- phase-night-1)
     ; collect any available actions from rooms and equipment
     ; that apply to this phase
     (while (p.phase != phase-day-2)
       (dbgl "\tprocessing " p.phase " for player " p.clientid)
       (def stuff
         (~>  
          (get-stuff-activations p)
          (map (λ (thing)
                 (tuple thing.name (add "Activate : " thing.desc)
                        (λ (thing.action thing p)))))))
       (case p.phase
         [phase-night-1
          (dbgl "\t\tovercome events")
          (p.phase <- phase-night-2)]
         [phase-night-2
          (dbgl "\t\tfeed survivors")
          (p.phase <- phase-night-3)]
         [phase-night-3
          (dbgl "\t\tmanage radioactivity")
          (p.phase <- phase-night-4)]
         [phase-night-4
          (dbgl "\t\trecruit survivors")
          (p.phase <- phase-night-5)]
         [phase-night-5
          (dbgl "\t\tbuild and activate rooms")
          (~>
           ; room building options at normal cost
           (get-building-actions p (λ (return _.cost)))
           (map (λ (room)
             (tuple
              room.name
              (add "Build : " room.desc " for cost " room.cost )
              (λ (begin
                   (dbgl "player chose " _)
                   (build-room p room (λ (return room.cost))))))))

           (append-many stuff)
           
           ; special end-phase choice
           (append-single
            (tuple "end-phase" "end this phase"
                   (λ (p.phase <- phase-night-6))))

           (flow-from-triple p.clientid "build a room?"))
          ]
         [phase-night-6
          (dbgl "\t\tfix equipment")
          (~>
           (get-fixing-actions p (λ (return (list-len _.cost))))
           (map (λ (equip)
             (tuple
              equip.name
              (add "Fix : " equip.desc " for cost " )
              (λ (begin
                   (dbgl "player chose " _)
                   (fix-equipment p equip (λ (return (list-len equip.cost)))))))))

           (append-many stuff)
           
           ; special end-phase choice
           (append-single
            (tuple "end-phase" "end this phase"
                   (λ (p.phase <- phase-night-7))))

           (flow-from-triple p.clientid "fix a piece of equipment?"))
          ]
         [phase-night-7
          (dbgl "\t\tshelter upkeep")
          (for (s shelter.stuff) (s.used <- #f))
          (for (h p.heroes) (h.moved <- #f))
          (p.phase <- phase-day-2)]))))

 ;main game loop
 (while (global.turn < 7)
   (dbgl "current turn is " global.turn)
   (case global.phase
     [phase-dawn
      (dbgl "processing dawn phase")
      (process-dawn)
      (global.phase <- phase-day)]
     [phase-day
      (dbgl "processing day phase")
      (process-day)
      (global.phase <- phase-night)]
     [phase-night
      (dbgl "processing night phase")
      (process-night)
      (global.phase <- phase-dawn)
      (global.turn += 1)])
   (flow-end))
      
  
 
 (dbgl "game over")
 'brk
 'brk
 (dbgl "end")
)

