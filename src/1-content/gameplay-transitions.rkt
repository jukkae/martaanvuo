#lang at-exp racket

(provide (all-defined-out))

(require
  "../0-engine/0-api/api.rkt"
  "narration/gameplay-transitions.rkt"
)

(define (on-begin-nth-run n)
  (case n
   [(1)
    '()]
   [(2)
    '()]
    )
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

  (current-round 0)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (when (= (current-recursion-depth) 1)
    (p @~a{
Otava stops her bike to correct her goggles on her way towards Martaanvuo wasteland. A dumb fucking plan, but she doesn't have much choice – the bill had finally come due. "Fifteen days", he had said, "two weeks and a one day extra as a courtesy".

The Merchant had demanded payment in actual gold, or assault weapons, as another fucking act of goodwill. So, after getting rid of the bracelet (3.8 grams), Otava is now chasing the rumor of a [cache] of valuables in Martaanvuo wasteland.
})
    (create-task 'the-debt)
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

           (set-task-status-text! debt-task (format "10.111 grams of gold (~a g paid)" (partially-completed-x completion)))

           (display-tasks)

           )

          (else
           (when (not (= (current-run) 0))
             (notice (format "End run number ~a [failed]" (number->string (current-run)))))))
)

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
Otava's bike roars and thunders as she speeds towards Martaanvuo wasteland. Morning light filters through arid, dusty air. She stops to put on goggles, against dust as much as against light.

The rumor says: In the wasteland ahead, near Martaanvuo dam, there's this basement laboratory, a fucking abandoned junkie cellar kitchen, and she'll find the [Anthead Monograph] there.

The Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation. Find the book that will fill in the blanks! Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh in the divine sausage machine, and out comes something magnificent:

Otava the Seeker, become Otava the Deathless!
})
     (create-task 'anthead-monograph)
     (wait-for-confirm)
     ]
    )
    )

(define (on-end-life)
  (display-end-of-life-summary)
  (wait-for-confirm))
