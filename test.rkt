#lang racket
(require "asm.rkt")
(require threading)
(require "core-lib.rkt")
(require syntax/parse/define)
(require (for-syntax racket/list))


(scurry
 (import core-lib)
 (def A (create-obj (["A" (list 1 2 3 4 5)])))
 (def B (create-obj (["A" (list 1 2 )])))

 (dbgl "A " (get-prop A "A" ) " B " (get-prop B "A"))
 
 (~ deal A "A" B "A" 2)

 (dbgl "A " (get-prop A "A" ) " B " (get-prop B "A"))
 )
