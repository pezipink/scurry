#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require "outlive-data.rkt")
(require threading)
(require syntax/parse/define)

(scurry
 (import core-lib)
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
      ["buildings" (create-starting-rooms)] ; todo: distribute advanced rooms
      ["equipment" (list)]
      
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
    
 ;(~> players (each dbg-obj))


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
     (while (p.phase <> phase-day-2)
       (dbgl "\tprocessing " p.phase " for player " p.clientid)
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
          (p.phase = phase-night-6)]
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
      (global.turn += 1)]))
      
  
 
 (dbgl "game over")
 'brk
 'brk
 (dbgl "end")
)


;; (def-λ (build-room state room-name cost)
;;    (dbgl "building room " room-name " for " cost)
;;    (def room (first (λ (eq _.name room-name)) state.unbuilt))
;;    (room.state = "built")
;;    (append-list state.single-use room)
;;    (state.materials -= (cost room))
;;    (flow-end)) 
 
;;  (def-λ (build-room-reqs state modifier)
;;    (def-λ (room->desc room)
;;     (add "Build room : " room.name ".  " room.desc " for cost " (modifier room)))
;;   (def-λ (can-build? room)
;;     (return
;;      (and
;;       (eq room.state "unbuilt")
;;       (lte state.materials (modifier room)))))
;;   (~>
;;    state.unbuilt  
;;    (filter can-build?)
;;    (map (λ (tuple
;;             _.name
;;             (room->desc _)
;;             (λ (name) (build-room state _.name modifier)))))))
