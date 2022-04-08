#lang at-exp racket

(provide (all-defined-out))

(require
  "../0-engine/0-api/api.rkt"
  "narration/gameplay-transitions.rkt"
)

(define (on-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (current-run (add1 (current-run)))
  (current-round 0)
  (advance-time-by-iotas! 35)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (when (= (current-run) 2)
    (p @~a{
  There's rumors that somewhere around Martaanvuo, there's this basement lab too, a fucking abandoned junkie cellar kitchen, and she'll find the [Anthead Monograph] there.

  The Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation. Find the book that will fill in the blanks. Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh into the divine sausage machine, and out comes something magnificent:

  Otava the Seeker, become Otava the Deathless!
 })
     (create-task 'anthead-monograph)
     (wait-for-confirm)
  )
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?)
  )

; recursions mess with reality -> change world state, give bonuses, open new doors
; but PC / instance / incarnation / 'life' continues
(define (on-begin-recurse-run)
  (current-run (add1 (current-run)))
  #;(current-round 0)
  (add-feature-to-location! (get-place-by-id 'martaanvuo-docks) 'mieli)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (narrate-begin-recurse-run))

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
  Otava's bike roars and thunders as she speeds through the badlands. Morning light filters through arid, dusty air. A hill from behind a hill, a curve after a curve, one much alike another. Here it still kind of feels like the Once-Was, the plains still the same, the freedom still the same. Damn it's good to be out on the plains again.

  Then she remembers the debt.

  The fucking debt, the bill finally come due, her reason for being here. "31 days", Mediator had said, "as an act of goodwill". And an insane amount of gold.
 })
     (create-task 'the-debt)
     (p @~a{
  Well, she's all kinds of insane.

  First of all, her research says, in the forests around Martaanvuo dam, there's a [cache] of valuables there. The canyon walls are criss-crossed by old tunnels dating back to the Once-Was, used by looters and scavengers and various unsavory elements of society through the years. Abandoned, ever since the blindscraper outbreak, but that was years ago. And these days this is *way* far out, so likely nobody's been there since.
 })
     (wait-for-confirm)
     ]
    )
    )

(define (on-end-life)
  (display-end-of-life-summary)
  (wait-for-confirm))
