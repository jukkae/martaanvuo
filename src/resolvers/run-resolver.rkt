#lang at-exp racket

(provide (all-defined-out))

(require
  "../core/io.rkt"
  "../core/utils.rkt"
  "../locations/0-types/location.rkt"
  "../locations/locations.rkt"
  "../pc/pc.rkt"
  "../quests/quest.rkt"
  "../quests/quests.rkt"
  "../round-resolver/round-resolver.rkt"
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
     (p @~a{
      Otava is following an old, overgrown path through the foggy woods. Late-morning gloom, forest like monochrome cardboard. Air thick with the damp smell of old, decaying forest.

      This should be worth it: There's a *cache* of valuables deep in the forest, but somewhere near it there's this basement lab too, a fucking abandoned junkie cellar kitchen called *the Maw*, and if what she knows and what she's figured out is correct, she'll find *Anthead Monograph* there.

      The first should net enough gold for Otava to pay back Mediator her debt anyway. That alone would be good enough a reason.

      The second one, though, Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation, and then none of this will matter anyway. The last step is to find the book that will fill in the blanks and make it all make sense. Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh into the divine sausage machine, and out comes something magnificent:

      Otava the Seeker, become Otava the Deathless!
      })
     ;(dev-note "Fixme: Quests")
     (wait-for-confirm)
     ;(create-quest 'pay-off-debt)
     ;(create-quest 'anthead-monograph)
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

(define (narrate-restart)
  (p
   (take-random ; TODO: probabilitify these kinds of take-random calls
    (list
     "In the depths of THE MAW, the Heart of the World stops. Then, it's all black."
     "In the depths of Murkwater-Aegis Workshop, Hartmann Device is turned on. False vacuum begins its collapse and space and time unfold until there is nothing."
     (string-append "The end."
                    "\n\n\n"
                    "M A R T A A N V U O"
                    "\n"
                    "==================="
                    "\n\n"
                    "Jukka Eerikäinen (2021)"
                    "\n\n")
     "[details omitted – 3 days later] Having passed  Martaanvuo, Otava comes upon an unnamed mountain range. She crosses over and begins a new life herding reindeer. She lives the rest of her days free from suffering and dies of natural causes at an elderly age."))))

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
     (narrate-restart)]
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
        ; end run?
        (when (eq? first-round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? first-round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? first-round-exit-status 'end-run) (end-run 'end-run))
        (when (eq? first-round-exit-status 'recurse) (end-run 'recurse))
        (when (eq? first-round-exit-status 'restart) (end-run 'restart))

        ; continue
        (when (eq? first-round-exit-status 'next-chapter) (next-chapter!)))

      (let loop ()
        (define round-exit-status (resolve-round 'begin))
        ; end run?
        (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? round-exit-status 'end-run) (end-run 'end-run))
        (when (eq? round-exit-status 'recurse) (end-run 'recurse))
        (when (eq? round-exit-status 'restart) (end-run 'restart))

        ; continue
        (when (eq? round-exit-status 'next-chapter) (next-chapter!))

        (loop))))
  (on-end-run run-exit-status)
  run-exit-status)
