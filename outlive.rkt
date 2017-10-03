#lang racket
(require "asm.rkt")
(require "core-lib.rkt")
(require threading)
(require syntax/parse/define)

(define-syntax-parser create-room
  [(_ name description cost capacity avail action others ...)
   #'(create-obj
      (["name" name]
       ["desc" description]
       ["type" "room"]
       ["capacity" capacity]
       ["state" "unbuilt"]
       ["workers" 0]
       ["cost" cost]         ; material build cost
       ["available" avail]
       ["action" action]
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
       ["available" avail]
       ["action" action]
       others ...
       ))])

(scurry
 (import core-lib)
 (def phase-startup "phase-startup")
 (def phase-night   "phase-night")
 (def phase-dawn    "phase-dawn")
 (def phase-day     "phase-day")
 
 (def-λ (create-shelter _)
   (create-obj
    (["metal" 0]["microchips" 0]
     ["water" 0]["material" 10])))
 
 (def-λ (build-room state room-name cost)
   (dbgl "building room " room-name " for " cost)
   (def room (first (λ (eq _.name room-name)) state.unbuilt))
   (room.state = "built")
   (append-list state.single-use room)
   (state.materials -= (cost room))
   (flow-end)) 
 
 (def-λ (build-room-reqs state modifier)
   (def-λ (room->desc room)
    (add "Build room : " room.name ".  " room.desc " for cost " (modifier room)))
  (def-λ (can-build? room)
    (return
     (and
      (eq room.state "unbuilt")
      (lte state.materials (modifier room)))))
  (~>
   state.unbuilt  
   (filter can-build?)
   (map (λ (tuple
            _.name
            (room->desc _)
            (λ (name) (build-room state _.name modifier)))))))

 (def-λ (remove-single-use state obj)
   (dbgl "remove single!")
   (remove-list state.single-use obj))

 (def room2
   (create-room
    "Room Construction"
    "Build a room with one less material (Room Construction special)"
    10
    3
    (λ (eq _.phase phase-night))
    (λ (state) (flow-from-triple
         "A" "Build a room (special)"
         (build-room-reqs state
                          (λ (room) (sub 1 room.cost)))))
    ))

 (dbg-obj room2)
 (def room3
   (create-room
    "Draw One Equipment"
    "Draw one equipment tile and fix an item with one less material (Draw One Equipment room special)"
    10
    3
    (λ (eq _.state phase-night))
    (λ (list ))))
    
 (def-obj state (["phase" phase-night]
                 ["unbuilt" (list room2 room3)]
                 ["materials" 10]
                 ["built" (list)]
                 ["single-use" (list)]))

 (def-λ (state-transition state)
   (cond
     [(eq state.phase phase-dawn)
      (state.phase = phase-day)]
     [(eq state.phase phase-day)
      (state.phase = phase-night)]
     [(eq state.phase phase-night)
      (state.phase = phase-dawn)]))
     
 (def-λ (normal-choices state)
  (extract-match ([(phase) state])
     [(eq _ phase-night)
      (list
       (tuple "build-room"
              "Build a room."
              (λ (flow-from-triple
                  "A" "build which room?"
                  (build-room-reqs state (λ (return _.cost))))))
       (tuple "fix-equipment"
              "Fix a piece of equipment"
              (list))
       (tuple "finish-night-phase"
              "Finish the night phase"
              (list)))]
     [(eq _ phase-day)
      (list
       (tuple "finish-day-phase"
              "Finish the day phase"
              (list)))]
     ))
       
 
 (def-λ (present-choices state)
   (def choices (normal-choices state))

   (~>
    state.single-use
    (filter (λ (_.available)))
    (each
     (λ (x)
       (append-list
        choices
        (tuple x.name x.desc
               (λ (begin
                    (x.action state)
                    (remove-single-use x.action state))))))))
                          
   ;; (foreach (x (~> state.single-use (filter (λ (_.available)))))
   ;;   (append-list choices
   ;;     (tuple x.name x.desc
   ;;            (λ (begin
   ;;                (x.action state)
   ;;                (remove-single-use x.action state))))))
   
   (flow-from-triple
    "A" "Choose an action"
    choices))
                    
   (dbgl "choice 1")
   (present-choices state)
   (dbg-obj state)
   (dbgl "choice 2")
   (present-choices state)
   (dbg-obj state)
   'brk
   (dbgl "end")
)
