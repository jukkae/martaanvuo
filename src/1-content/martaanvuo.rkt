#lang at-exp racket

(provide (all-defined-out))

(require
  "../0-engine/0-api/api.rkt"
  "narration/martaanvuo.rkt"
)

; Main entrypoint of campaign
(define (on-begin-playthrough!)
  (reset-world!)
  (on-begin-life)
  (on-begin-run))


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
  (advance-time-by-iotas! (d 6 12))
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
    )
  (when (= (current-recursion-depth) 2)
    (p @~a{
Murkwater guards should be out, time to storm the facility at Martaanvuo Dam. Break in, find the gold, find the armory, grab the guns. Useful for the Ant Legion – the Resistance of the Wasteland. Find the reactor chamber, set the charges, get the fuck out. Some automated resistance is expected, but other than that, should be easy.
})
    (create-task 'storm-the-facility)
    (wait-for-confirm)
    )
  (when (= (current-recursion-depth) 3)
    (p @~a{
The Hartman Device, the ultimate weapon of mass destruction: When activated, it initiates a null-field vacuum collapse, smoothing out the crinkled fabric of reality. The reaction will proceed outwards, destroying the very structure of space itself, and all reality will be permanently reduced to nothing – an ultimate kind of nothing, a void in which nothing *could* exist.

A terrorist group, led by the Magpie King, is operating from the Maw of Martaanvuo. It is believed that they are turning a transporter machine into a Hartman Device. The terrorist group must be neutralized, and the device defused and kept intact.
})
    (create-task 'defuse-the-hartman-device)
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
    [(2)
     (p @~a{
Otava is riding a roaring beast through the desolate wastelands of Martaanvuo. Dried up riverbeds, starving cattle, starving people. Desperate people.

There's a waterfall far upriver, and surrounding the waterfall, there are caves there that go deep into the darks of the earth, to the heart of the world, with evil beings and innumerable treasures. With the treasures, she could do good.
})
     (create-task 'plunder-the-earth)
     (wait-for-confirm)
     ]
    [(3)
     (p @~a{
Otava is riding a roaring beast through the desolate wastelands of Martaanvuo. Dried up riverbeds, starving cattle, starving people. Desperate people.

She, Otava the Seeker, is seeking a cure. Cure to suffering, cure to death. Such a cure can be found, the sages say, on the pages of the Anthead Monograph. The Monograph is buried in the Maw of Martaanvuo.

The Fox of the Waterfall knows more. The Waterfall is upriver Martaanvuo.
})
     (create-task 'fox-of-the-waterfall)
     (wait-for-confirm)
     ]
    [(4)
     (p @~a{
The ants are becoming conscious. There's an vile mind-control fungus roaming, infecting them, shaping and bending them, and spreading to larger animals too.

It's originating from somewhere near the Waterfall. The origin must be found, burned to the ground, buried, destroyed.
})
     (create-task 'destroy the-origin)
     (wait-for-confirm)
     ]
    [(5)
     (p @~a{
There's something really fucky going with Martaanvuo. Ever since Murkwater moved in with their trucks and heavy equipment, and dammed the Martaanvuo Waterfall, there's been strange rumors circulating among the peoples living in the Wastelands.
})
     (wait-for-confirm)
     ]
    )
    )

(define (on-end-life)
  (display-end-of-life-summary)
  (wait-for-confirm))
