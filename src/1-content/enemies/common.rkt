#lang at-exp racket

(provide make-melee-action approach-action get-skip-action retreat-action try-to-escape)

(require
  "../../0-engine/0-api/api.rkt"
  )

(define (make-melee-action actor)
  (make-melee-attack-action
   #:actor actor
   #:duration 1
   #:target 'pc
   #:n 1
   #:x 2
   #:bonus -1
   )
  )

(define (approach-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['adjacent 'engaged]
      ['close 'adjacent]
      ['nearby 'close]
      ['far 'nearby]))
  (define subject (actor-id actor))
  (make-action
   #:symbol 'approach
   #:actor actor
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-actor-stance-range! (get-actor ,subject) ',next-range #f)
     'ok)
   #:details '(slow)))

(define (get-skip-action actor)
  (make-action
   #:symbol 'skip
   #:actor actor
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:details '(slow silent)))

(define (retreat-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['engaged 'adjacent]
      ['adjacent 'close]
      ['close 'nearby]
      ['nearby 'far]))
  (define subject (actor-id actor))
  (make-action
   #:symbol 'retreat
   #:actor actor
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-actor-stance-range! (get-actor ,subject) ',next-range)
     'ok)
   #:details '(slow)))

(define (escape-action actor)
  (define id (actor-id actor))
  (make-action
   #:symbol 'escape
   #:actor actor
   #:duration 1
   #:target '()
   #:tags '(initiative-based-resolution fast)
   #:details '()
   #:resolution-rules
   `(
     (notice (format "The ~a escapes" (get-combatant-name (get-actor ,id))))
     (award-xp! 1)
     (remove-actor-from-its-current-location! (get-actor ,(actor-id actor)))
    'ok
     ))
  )

(define (try-to-escape actor)
  (cond [(or (equal? (actor-stance-range actor) 'engaged)
             (equal? (actor-stance-range actor) 'adjacent)
             (equal? (actor-stance-range actor) 'close))
         (retreat-action actor)]
        [(equal? (actor-stance-range actor) 'nearby)
         (retreat-action actor)]
        [(equal? (actor-stance-range actor) 'far)
         (escape-action actor)])
  )
