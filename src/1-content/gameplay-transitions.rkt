#lang at-exp racket

(provide (all-defined-out))

(require
  "../0-engine/0-api/api.rkt"
  "narration/gameplay-transitions.rkt"
)

(define (on-begin-nth-run n)
  (case n
   [(1)
    (go-to-fragment 'narrow-bridge)]
   [(2)
    (p @~a{
She can't stop thinking about the gas station attendant, and her message that was to be delivered to the termites: The Hartmann device is in the workshop. Not her business, but...
    })
    (create-task 'the-message)
    (p "Fly. Swat.")
    (wait-for-confirm)])
)

(define (on-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (current-run (add1 (current-run)))
  (current-round 0)
  (advance-time-by-iotas! 35)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (on-begin-nth-run (current-run))
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?)
  )

; recursions mess with reality -> change world state, give bonuses, open new doors
; but PC / instance / incarnation / 'life' continues
(define (on-begin-recurse-run)
  (current-recursion-depth (add1 (current-recursion-depth)))

  #;(current-round 0)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (when (= (current-recursion-depth) 1)
    (next-chapter!)
    (p @~a{
Martaanvuo. According to rumors, there's this basement lab here somewhere near the dam, an abandoned junkie cellar kitchen, and she'll find the [Anthead Monograph] there.

The Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation. Find the book that will fill in the blanks. Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh into the divine sausage machine, and out comes something magnificent:

Otava the Seeker, become Otava the Deathless!
    })
    (create-task 'anthead-monograph)
    (p "Fly. Swat.")
    (wait-for-confirm)
    ))

(define (on-end-run exit-status)
  (reset-pending-action!)
  (when (and (not (eq? exit-status 'restart))
             (not (eq? exit-status 'recurse)))
    (cond ((> (pc-gold-amount) 0)
           (define debt-task (task-exists? 'the-debt))

           (define gold-collected (pc-gold-amount))

           (display-run-summary)
           (wait-for-confirm)

           (remove-item! 'gold #:quantity-to-remove 'all)
           (define completion (task-state debt-task))
           (match completion
            [(partially-completed x y)
             (set-partially-completed-x! completion (+ x gold-collected))])

           (set-task-status-text! debt-task (format "10.111 grams of gold (~a paid)" (partially-completed-x completion)))

           (display-tasks)

           )

          (else
           (notice (format "End run number ~a [failed]" (number->string (current-run)))))))

  (case exit-status
    ['end-run
     ;(p "She's still alive.")
     '()]
    ['recurse
     (blurb 'martaanvuo-title)]
    ['restart
     '()]
    ['pc-dead '()]
    [else
     (dev-note (format "on-end-run: unhandled exit status: ~a" exit-status))])

  (when (not (eq? exit-status 'pc-dead))
    (wait-for-confirm)))


; the world and reality stays intact, world state persists
; eventually, Otava should become aware of the loop
(define (on-begin-life)
  (when (not (session-flag-set? 'began-life))
    (set-session-flag 'began-life)
    (current-session-score-dice++)
    )

  (current-life (add1 (current-life)))
  (current-pc (make-new-pc))
  (set-base-build!)
  (when (not (= 1 (current-life)))
    (dev-note "Show life info"))

  (case (current-life)
    [(1)
     (p @~a{
  Otava's bike roars and thunders as she speeds through the badlands. Morning light filters through arid, dusty air. A hill after a hill, a curve after a curve, here it still kind of feels like the Once-Was, the plains still the same, the air still the same.

  Her being here, riding towards the rotten, cursed Martaanvuo wasteland, definitely caused by running out of good options. But, now that she's here, there is definitely a many flies – single swat situation.

  The debt. "15 days", Mediator had said, "two weeks and one day extra, as an act of goodwill". And, frankly speaking, a rather worrisome sum total.
 })
     (create-task 'the-debt)
     (p @~a{
  Mediator wouldn't take payment in anything other than gold, so, after getting rid of the bracelet, Otava is now chasing the rumor of a [cache] of valuables in the wasteland.

  Fly. Swat.
 })
     (wait-for-confirm)
     ]
    )
    )

(define (on-end-life)
  (display-end-of-life-summary)
  (wait-for-confirm))
