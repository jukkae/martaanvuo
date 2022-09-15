#lang at-exp racket

(provide approach-action get-skip-action retreat-action)

(require
  "../../0-engine/0-api/api.rkt"
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
