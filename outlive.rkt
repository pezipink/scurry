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
 (foreach (p players)
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

   (dbg-obj shelter)
   
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
   
 

 (~> players (each dbg-obj))

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
