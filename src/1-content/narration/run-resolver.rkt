#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"
  "../../0-engine/4-rules/blurbs/blurbs.rkt"
  "../../0-engine/4-rules/tasks/task.rkt"
  "../../0-engine/7-state/state/state.rkt"
  "../../0-engine/7-state/state/logging.rkt"
  )

(define (narrate-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (when (not suppress-new-chapter?) (next-chapter!))

  ; Don't show this until the second run!
  (when (not (= 1 (current-run)))
    (notice (format "Begin run number ~a" (current-run))))
  )

(define (narrate-begin-recurse-run)
  (next-chapter!)

  (p @~a{
    Otava is on Mediator's path in the foggy cardboard cutout woods. She gets to Fork and Anthill.
  }))
