#lang racket
(require "asm.rkt")
(require threading)
(require "core-lib.rkt")
(require syntax/parse/define)
(require (for-syntax racket/list))


(scurry
  (import core-lib)
  (def x 10)
  (def x 11)
  (def y (list 1 2 3 4))
  (foreach (i y)
    (dbgl i))

  (def-λ (mod-global-list mapper key)
    (def list (get-global key))
    (~ mapper list)
    (set-global key list))

  (def z (add x i))

  )
