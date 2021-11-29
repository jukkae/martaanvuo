#lang racket

(provide (all-defined-out))

(require "actor.rkt"
         "../core/io.rkt"
         "../core/utils.rkt")

(require racket/serialize)

(serializable-struct
 pc-actor
 actor
 ([lp #:mutable]
  [max-lp #:mutable]
  [death-roll-dice #:mutable]
  [alive? #:mutable]
  [cause-of-death #:mutable]
  [xp #:mutable]
  [hunger #:mutable])
 #:constructor-name pc-actor*)

 (define (make-pc-actor
         name
         max-hp
         max-lp)
  (pc-actor*
   name max-hp max-hp
   ; attributes
   '() '() '() '() '()
   ; traits etc
   (make-hash) '() '() '() '() '() max-lp max-lp 6 #t '() 0
   ; hunger
   200))

(define (pc-take-damage! actor damage damage-type)
  (when (< damage 0) (error "pc-take-damage: damage cannot be less than 0"))

  (cond ((not (positive? (actor-hp actor)))

         (define new-hp (- (actor-hp actor) damage))
         (set-actor-hp! actor new-hp)
         (notice (format "Taking damage, new HP : ~a]" new-hp))

         (define death-roll-dice (pc-actor-death-roll-dice actor))
         (define death-roll (d 1 death-roll-dice))
         (define result (+ death-roll
                           (actor-hp actor)))
         (notice (format "Death roll: 1d~a + HP = ~a – ~a = ~a"
                         death-roll-dice
                         death-roll
                         (abs (actor-hp actor)) ; slightly dirty: actor-hp *should* be non-positive
                         result))

         (define cause-of-death damage-type)

         (cond ((<= result 1)
                (begin
                  (kill actor cause-of-death)
                  'dead))
               (else
                'hit)
               ))

        (else
         (define new-hp (- (actor-hp actor) damage))
         (when (not (positive? new-hp))
           (displayln "[Otava is dying.]")
           (wait-for-confirm))

         (set-actor-hp! actor new-hp)
         'hit)))
