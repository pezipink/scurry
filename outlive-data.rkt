#lang racket
(require "asm.rkt")
(require (for-syntax syntax/parse))
(require syntax/parse/define)
(require threading)
(provide (all-defined-out))

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
           (gte this.workers this.capacity)
           (eq this.used? #f)
           (avail this state)))]
       ["action"
        (λ (this state)
          (this.used?)
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
          (this.used?)
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

      (def-λ (create-airlock)
        ; the airlock is special and is never "activated"
        ; and its supply cost is calcuated elsewhere
        (create-room "airlock" "the airlock" 0 0 7
                       (λ (this state) (return #f))
                       (λ (this state) (dbgl "!"))))
      
      (def-λ (create-starting-rooms)
        (def equip
          (create-room "draw-equip-repair"
                       "draw one equipment tile and repair for 1 material less"
                       3 2 3
                       (λ (this state)
                         (eq state.phase phase-night-6))
                       (λ (this state)
                         (dbgl "draw-equip-repair was selected"))
                       ))

        (def construct
          (create-room "room-construction"
                       "build one room with 2 less material"
                       3 2 3
                       (λ (this state)
                         (eq state.phase phase-night-5))
                       (λ (this state)
                         (dbgl "room-construction was selected")
                         )))

        ;todo: overcome event
        (return (list equip construct))
        )


))
