#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")
; see also lazy-require and units

(define pc%
  (class* object% ()
    (field [hp 4])
    (field [inventory null])
    (super-new)

    (define/public (get-next-action) (error "not implemented yet!"))
    (define/public (hit dmg) (displayln "OUCH (yes, that was you"))
    (define/public (get-status) (error "not implemented yet!"))))

(provide (all-defined-out))