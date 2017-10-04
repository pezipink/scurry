#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require "outlive-data.rkt")
(require threading)
(require syntax/parse/define)

(scurry
 (import core-lib)

 (def-λ (pay-cost player-state cost-type amount)   
   (def paid 0)
   (def shelter player-state.shelter)
   
   (def-λ (create-req type)
     (tuple type
            (add "pay 1 " type "(" (get-prop shelter type) " remaining)")
            (λ (prop-= shelter type 1))))

   (while (lt paid amount)
     (~>
      (case cost-type
        ["materials" (list "wood" "metal" "microchips")]
        ["supplies"  (list "water" "meat" "canned-goods")]
        [else (list cost-type)])
      (filter (λ (gt (get-prop shelter _) 0)))
      (map create-req)
      (flow-from-triple
        player-state.clientid
        (add "pay the cost (" paid "/" amount ")")))

     (++ paid)))

 (def-λ (shelter-has-enough shelter cost-type amount)
   (dbgl "in shelter-has-enough " amount)
   (case cost-type
     ["materials"
      (return (gte (add shelter.wood shelter.metal shelter.microchips) amount))]))
 
 
 (def-λ (build-room player-state room cost-modifier)
   (dbgl "in build-room")
   (pay-cost player-state "materials" (cost-modifier room))
   (room.state = "built")   
   (flow-end)
   )
 
 (def-λ (get-building-actions player-state cost-modifier)
   (dbgl "in get building actions")
   (def shelter player-state.shelter)
   (~>
    shelter.stuff
    (filter (λ (and (eq _.type "room") (eq _.state "unbuilt"))))
    (filter (λ (shelter-has-enough
                shelter "materials" (cost-modifier _)))))
       
   )

  (def-λ (get-stuff-activations player-state)
   (dbgl "in get stuff activations")
   (def shelter player-state.shelter)
   (~>
    shelter.stuff
    (filter (λ (return (_.avail? _ player-state))))))


 (import outlive-data)

 (def players (vals (get-players)))
 
 (global.turn = 0)
 (global.resources = (list))
 (global.phase = phase-dawn)
 (global.events = (list))
 (global.equipment = (list))

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
      ["microchips" 2]
      ["metal" 2]
      ["wood" 1]

      ["radioactivity" 0]
      ["leader" ""] ;todo
      
      ))

   ;(dbg-obj shelter)
   
   (def-obj action-data
     (["used-actions" 0]
      ["last-result" ""]))

   ;todo: assign leader and resources
   (def starting-loc "home") ;todo: leader dependent

   (set-props p
     (["shelter" shelter]
      ["phase" phase-day-2]
      ["heroes"
        (list
         (create-hero starting-loc 3)
         (create-hero starting-loc 3)
         (create-hero starting-loc 4)
         (create-hero starting-loc 5))]
      ["action-data" action-data]))

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
     (p.phase = phase-day-2)
     (for (h p.heroes)      
      (dbgl "\tprocessing hero " h.strength " at location " h.location)))

   )
 
 (def-λ (process-night)
   ;event resolution, feeding, radioactivity, building, fixing
   ; todo: this can be done concurrently for each player   
   (dbgl "process-night")

   (for (p players)                   
     (p.phase = phase-night-1)
     ; collect any available actions from rooms and equipment
     ; that apply to this phase
     (while (p.phase <> phase-day-2)
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
          (p.phase = phase-night-2)]
         [phase-night-2
          (dbgl "\t\tfeed survivors")
          (p.phase = phase-night-3)]
         [phase-night-3
          (dbgl "\t\tmanage radioactivity")
          (p.phase = phase-night-4)]
         [phase-night-4
          (dbgl "\t\trecruit survivors")
          (p.phase = phase-night-5)]
         [phase-night-5
          (dbgl "\t\tbuid and activate rooms")
          (~>
           ; room building options at normal cost
           (get-building-actions p (λ (return _.cost)))
           (map (λ (room)
             (tuple
              room.name
              (add "Build : " room.desc " for cost " room.cost )
              (λ (begin
                   (dbgl "player chose " _)
                   (build-room p room (λ (return _.cost))))))))

           ;; ; any available building actions
           (append-many stuff)
           
           ; special end-phase choice
           (append-single
            (tuple "end-phase" "end this phase"
                   (λ (p.phase = phase-night-6))))

           (flow-from-triple p.clientid "build a room?"))
          ]
         [phase-night-6
          (dbgl "\t\tfix equipment")
          (p.phase = phase-night-7)]
         [phase-night-7
          (dbgl "\t\tshelter upkeep")
          
          (p.phase = phase-day-2)]))))

 ;main game loop
 (while (global.turn < 7)
   (dbgl "current turn is " global.turn)
   (case global.phase
     [phase-dawn
      (dbgl "processing dawn phase")
      (process-dawn)
      (global.phase = phase-day)]
     [phase-day
      (dbgl "processing day phase")
      (process-day)
      (global.phase = phase-night)]
     [phase-night
      (dbgl "processing night phase")
      (process-night)
      (global.phase = phase-dawn)
      (global.turn += 1)])
   (flow-end))
      
  
 
 (dbgl "game over")
 'brk
 'brk
 (dbgl "end")
)
