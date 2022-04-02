#lang at-exp racket

(provide (all-defined-out))

(require
  "../3-round-resolver/round-resolver.rkt"

  "../../2-core/core.rkt"
  "../../2-core/io.rkt"

  "../../3-types/location.rkt"

  "../../4-systems/blurbs/blurbs.rkt"
  "../../4-systems/locations/locations.rkt"
  "../../4-systems/pc/pc.rkt"
  "../../4-systems/tasks/tasks.rkt"
  "../../4-systems/world/world.rkt"

  "../../7-state/state/state.rkt"
  "../../7-state/state/logging.rkt"

  "../../../1-content/narration/run-resolver.rkt"
  )

; engine / run-resolver
(define (on-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (current-run (add1 (current-run)))
  (current-round 0)
  (remove-flag 'ending-run-allowed)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?)

  (case (current-run)
    [(1)
     (blurb 'begin-first-run-pt-1)
     (create-task 'pay-off-debt)
     (wait-for-confirm)
     (blurb 'begin-first-run-pt-2)
     (create-task 'anthead-monograph)
     (wait-for-confirm)
     ])
  )

; recursions mess with reality -> change world state, give bonuses, open new doors
; but PC / instance / incarnation / 'life' continues
(define (on-begin-recurse-run)
  (current-run (add1 (current-run)))
  #;(current-round 0)
  (remove-flag 'ending-run-allowed)
  (add-feature-to-location! (get-place-by-id 'martaanvuo-docks) 'mieli)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (narrate-begin-recurse-run))

(define (on-continue-run)
  '())

(define (on-end-run exit-status)
  (reset-pending-action!)
  (when (and (not (eq? exit-status 'restart))
             (not (eq? exit-status 'recurse)))
    (cond ((> (pc-gold-amount) 0)
           (define debt-task (find-task 'pay-off-debt))
           (define gold-collected (pc-gold-amount))
           (reduce-debt-by! gold-collected)
           (remove-item! 'gold)

           (displayln "Task:")
           (displayln debt-task)

           (display-run-summary))

          (else
           (notice (format "End run number ~a [failed]" (number->string (current-run)))))))

  (case exit-status
    ['end-run
     (p "She's still alive.")]
    ['recurse
      (blurb 'martaanvuo-title)]
    ['restart
     '()]
    ['pc-dead '()]
    [else
     (dev-note (format "on-end-run: unhandled exit status: ~a" exit-status))])

  (when (not (eq? exit-status 'pc-dead))
    (wait-for-confirm)))

; engine / run-resolver
(define (resolve-run mode
                     #:suppress-new-chapter? [suppress-new-chapter? #f])

  (case mode
    ['continue (on-continue-run)]
    ['begin (on-begin-run #:suppress-new-chapter? suppress-new-chapter?)]
    ['restart (on-begin-run #:suppress-new-chapter? suppress-new-chapter?)]
    ['recurse (on-begin-recurse-run)]
    [else (dev-note "Unknown resolve-run-mode!") (error "fix this")])

  (define run-exit-status
    (let/ec end-run
      (when (eq? mode 'continue)
        (define first-round-exit-status (resolve-round 'continue))
        (case first-round-exit-status
          ; end run
          ['pc-dead (end-run 'pc-dead)]
          ['win-game (end-run 'win-game)]
          ['end-run (end-run 'end-run)]
          ['recurse (end-run 'recurse)]
          ['restart (end-run 'restart)]
          ; continue
          ['next-chapter (end-run 'next-chapter!)]))

      (let loop ()
        (define round-exit-status (resolve-round 'begin))
        (case round-exit-status
          ; end run
          ['pc-dead (end-run 'pc-dead)]
          ['win-game (end-run 'win-game)]
          ['end-run (end-run 'end-run)]
          ['recurse (end-run 'recurse)]
          ['restart (end-run 'restart)]
          ; continue
          ['next-chapter (end-run 'next-chapter!)])

        (loop))))
  (on-end-run run-exit-status)
  run-exit-status)
