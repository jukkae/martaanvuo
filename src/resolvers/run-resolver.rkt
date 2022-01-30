#lang at-exp racket

(provide (all-defined-out))

(require
  "round-resolver/round-resolver.rkt"
  "../blurbs/blurbs.rkt"
  "../core/io.rkt"
  "../core/utils.rkt"
  "../locations/0-types/location.rkt"
  "../locations/locations.rkt"
  "../pc/pc.rkt"
  "../quests/quest.rkt"
  "../quests/quests.rkt"
  "../state/state.rkt"
  "../state/logging.rkt"
  "../world/world.rkt")

; content should be provided "somewhere"
; content is game-specific, not engine stuff
; but figure out where this should be called from
; engine / run-resolver
(define (narrate-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (when (not suppress-new-chapter?) (next-chapter!))

  ; Don't show this until the second run!
  (when (not (= 1 (current-run)))
    (info-card
     (list
      (list " run " (string-append " " (number->string (current-run)) " ")))
     (string-append "Begin run number " (number->string (current-run)))))


  (case (current-run)
    [(1)
     (blurb 'begin-first-run)
     ;(dev-note "Fixme: Quests")
     (wait-for-confirm)
     (create-quest 'pay-off-debt)
     (create-quest 'anthead-monograph)
     ]))

(define (narrate-begin-recurse-run)
  (next-chapter!)

  (p @~a{
    Otava is on Mediator's path in the foggy cardboard cutout woods. She gets to Fork and Anthill.
  }))
  ; Otava is on Brokerstrail and comes to Fork-and-Anthill BUT THIS TIME KNOWY OF HARTMAN-DEVICE
  ; sort of like "worlds" 1-2-3

; engine / run-resolver
(define (on-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (current-run (add1 (current-run)))
  (current-round 0)
  (remove-flag 'ending-run-allowed)
  (move-pc-to-location! (find-place-by-id (world-places (current-world)) 'perimeter))
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?))

(define (on-begin-recurse-run)
  (current-run (add1 (current-run)))
  #;(current-round 0)
  (remove-flag 'ending-run-allowed)
  (add-feature-to-location! (find-place-by-id (world-places (current-world)) 'martaanvuo-docks) 'mieli)
  (move-pc-to-location! (find-place-by-id (world-places (current-world)) 'perimeter))
  (narrate-begin-recurse-run))

(define (on-continue-run)
  '())

(define (on-end-run exit-status)
  (when (and (not (eq? exit-status 'restart))
             (not (eq? exit-status 'recurse)))
    (cond ((> (pc-gold-amount) 0)
           (define debt-quest (find-quest 'pay-off-debt))
           (define gold-collected (pc-gold-amount))
           (reduce-debt-by! gold-collected)
           (remove-item! 'gold)

           (displayln "Quest:")
           (displayln debt-quest)

           (info-card
            (list
             (list " run "
                   (string-append " " (number->string (current-run)) " "))
             (list " gold collected "
                   (string-append " " (number->string (pc-gold-amount)) " grams "))
             (list " debt still owed "
                   (string-append " " (number->string (quest-details debt-quest)) " grams ")))
            (string-append "Run number " (number->string (current-run)) " ended")))

          (else
           (notice (string-append
                      "End run number "
                      (number->string (current-run))
                      " [failed]")))))

  (case exit-status
    ['end-run
     (p "She's still alive.")]
    ['recurse
     (define title-string
       (string-append "M A R T A A N V U O"
                      "\n"
                      "==================="))
     (p title-string)]
    ['restart
     '()]
    ['pc-dead '()]
    [else
     (p "narrate-end-run: unhandled exit status: " (symbol->string exit-status))])

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
