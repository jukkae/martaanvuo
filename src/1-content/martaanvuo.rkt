#lang at-exp racket/base

(provide (all-defined-out))

(require
  racket/match
  "../0-engine/0-api/api.rkt"
  "narration/gameplay-transitions.rkt"
  "world/world.rkt"
)

(define (setup-world! scenario)
  (current-world (make-new-world))
  (dynamic-require "src/1-content/world/locations/arena.rkt" #f)
  (define the-maw (get-location-by-id 'the-maw))
  (when (not (flag-set? 'notebook-added))
    (add-item-to-location!
      the-maw
      (new-item
        (Name "notebook" "a" "'s" "notebooks")
        #:id 'notebook))
    (set-flag 'notebook-added)
    )
  )

; Main entrypoint of campaign
(define (on-begin-playthrough! scenario)
  (reset-state!)
  (setup-world! scenario)

  ; housekeeping – TODO: semantically different from post-pc-death
  (on-begin-life)
  (on-begin-run #:suppress-new-chapter? #t))


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
  (on-begin-nth-run (current-run))
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?)
  )

; recursions mess with reality -> change world state, give bonuses, open new doors
; but PC / instance / incarnation / 'life' continues
(define (on-begin-recurse-run)
  (when (flag-set? 'scenario-evolution)
    (wait-for-confirm)
    )

  (p title-string)

  (p ", the title concludes.")
  (current-recursion-depth (add1 (current-recursion-depth)))

  (remove-feature-from-location! (current-location) 'the-button)
  (remove-feature-from-location! (current-location) 'light-switch)

  (wait-for-confirm)

  (when (flag-set? 'scenario-evolution)

    @p{
There's something called the Lamarck process, which is what drives the changes in organisms in Martaanvuo. To control the Lamarck process is to control the changes. And to control the changes is to control life. But to control it, it must be understood.
    }
    (create-task 'evolve)

    @p{
The first test subject the notebook follows is an "organic tissue adaptive-volitional amoeboid" #1.
    }

    (set-pc-actor-sense-organs! (pc) '())
    (set-pc-actor-manipulators! (pc) '())
    (set-actor-size! (pc) 'small)
    (define silent? #t)
    (add-sense-organ! (SenseOrgan 'chemical-gradient-detector "chemical gradient detector") silent?)
    (add-manipulator! (Manipulator 'pseudopodia "cytoplasmic pseudopodia") silent?)
    )

  ; (current-round 0)
  ; (move-pc-to-location! (get-place-by-id 'perimeter))
  ; (case (current-recursion-depth)
  ;   [(1)
  ;    @p{
  ;     Otava is at the perimeter of Martaanvuo wasteland. Her plan is a dumb fucking one for sure, but the bill has come due. "Fifteen days", he had said, "two weeks and a one day extra as an act of goodwill".

  ;     So, after getting rid of the bracelet (3 grams), Otava is now chasing the rumor of a [cache] of valuables in Martaanvuo wasteland, somewhere near the dam.
  ;     }
  ;    (create-task 'the-debt)
  ;    (wait-for-confirm)
  ;    ]
  ;   ; [(2)
  ;   ;  (p @~a{
  ;   ;   Murkwater guards should be out, time to storm the facility at Martaanvuo Dam. Break in, find the gold, find the armory, grab the guns. Useful for the Ant Legion – the Resistance of the Wasteland. Find the reactor chamber, set the charges, get the fuck out. Some automated resistance is expected, but other than that, should be easy.
  ;   ;   })
  ;   ;  (create-task 'storm-the-facility)
  ;   ;  (wait-for-confirm)
  ;   ;  ]
  ;   ; [(3)
  ;   ;  (p @~a{
  ;   ;   The Hartman Device, the ultimate weapon of mass destruction: When activated, it initiates a null-field vacuum collapse, smoothing out the crinkled fabric of reality. The reaction will proceed outwards, destroying the very structure of space itself, and all reality will be permanently reduced to nothing – an ultimate kind of nothing, a void in which nothing *could* exist.

  ;   ;   A terrorist group, led by the Magpie King, is operating from the Maw of Martaanvuo. It is believed that they are turning a transporter machine into a Hartman Device. The terrorist group must be neutralized, and the device defused and kept intact.
  ;   ;   })
  ;   ;  (create-task 'defuse-the-hartman-device)
  ;   ;  (wait-for-confirm)
  ;   ;  ]
  ;   )
  ; (when (not (pc-has-sense-organ? 'eyes))
  ;   (p "There's a dim flat array of amorphous shapes. Fuzzy forms turn more solid. Otava begins to see.")
  ;   (add-sense-organ! (SenseOrgan 'eyes "eyes"))
  ;   (wait-for-confirm))
  ; (when (pc-has-sense-organ? 'sonar)
  ;   (p "The afterimages of the shadowy outline of the world fade. Otava feels untethered.")
  ;   (remove-sense-organ! 'sonar)
  ;   (wait-for-confirm))
  ; (when (pc-has-sense-organ? 'ears)
  ;   (p "Noise of the world fades into silence.")
  ;   (remove-sense-organ! 'ears)
  ;   (wait-for-confirm))
  )

(define (on-end-run exit-status)
  (reset-pending-action!)
  (when (and (not (equal? exit-status 'restart))
             (not (equal? exit-status 'recurse)))
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

  (when (not (equal? exit-status 'pc-dead))
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
     @p{
      Otava is at the perimeter of Martaanvuo wasteland. There's a rumor saying that upriver Martaanvuo, near the dam, there's this basement laboratory called the Maw, a fucking abandoned junkie cellar kitchen, and she'll find the [Anthead Monograph] there.

      The Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation. Find the book that will fill in the blanks! Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the clockwork of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh in the divine sausage machine, and out comes something magnificent:

      Otava the Seeker, become Otava the Deathless!
     }
     (create-task 'anthead-monograph)
     (wait-for-confirm)
     (advance-time-by-iotas! (d 6 12))
     (notice "Some time passes.")
     (next-chapter!
       (string-append
         (take-random (list " – " ": " " "))
         (take-random (list "in which lessons are learned"
                            "in which a lesson is learned"
                            "a lesson is learned"
                            "some lessons are learned"
                            "lessons are learned"))))
     (move-pc-to-location! (get-place-by-id 'the-maw))
    ;  (spawn-encounter)
     ]
    [else
     (advance-time-by-iotas! (d 6 12))
     (notice "Some time passes.")
     (move-pc-to-location! (get-place-by-id 'the-maw))
     (spawn-encounter)
     ]
  ;   [(2)
  ;    @p{
  ;     Otava is riding a roaring beast through the desolate wastelands of Martaanvuo. Dried up riverbeds, starving cattle, starving people.

  ;     There's a waterfall far upriver, and surrounding the waterfall, there are caves there that go deep into the darks of the earth, to the heart of the world, full of crawling evil beings and innumerable treasures.
  ;    }
  ;    (create-task 'plunder-the-earth)
  ;    (wait-for-confirm)
  ;    ]
  ;   [(3)
  ;    @p{
  ;     There's something really fucky going with Martaanvuo. Ever since Murkwater moved in with their trucks and heavy equipment and dammed the Martaanvuo Waterfall, there's been strange rumors circulating among the peoples living in the Wastelands.
  ;     }
  ;    (wait-for-confirm)
  ;  ]
    )
    )

(define (on-end-life)
  (display-end-of-life-summary)
  (wait-for-confirm))
