#lang at-exp racket

(provide get-closer-action get-skip-action get-further-action)

(require
  "../../0-engine/0-api/api.rkt"
  )

(define (get-closer-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['adjacent 'engaged]
      ['close 'adjacent]
      ['nearby 'close]
      ['far 'nearby]))
  (define subject (actor-id actor))
  (make-action
   #:symbol 'get-closer
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

(define (get-further-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['engaged 'adjacent]
      ['adjacent 'close]
      ['close 'nearby]
      ['nearby 'far]))
  (define subject (actor-id actor))
  (make-action
   #:symbol 'get-further
   #:actor actor
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-actor-stance-range! (get-actor ,subject) ',next-range)
     'ok)
   #:details '(slow)))
