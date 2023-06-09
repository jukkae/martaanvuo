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
  ; to load additional locations:
  ; (dynamic-require "src/1-content/world/locations/arena.rkt" #f)
  (define the-maw (get-location-by-id 'the-maw))
  (when (not (flag-set? 'notebook-added))
    (add-item-to-location!
      the-maw
      (new-item
        (Name "book" "a" "'s" "books")
        #:id 'book))
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

(define (on-begin-evolution)
  (wait-for-confirm)

  (p title-string)

  (p ", the title concludes.")

  (remove-feature-from-location! (current-location) 'the-button)

  (wait-for-confirm)

  @p{
The first test subject the notebook follows is an "organic tissue adaptive-volitional amoeboid" #1.
}

  (set-pc-actor-sense-organs! (pc) '())
  (set-pc-actor-manipulators! (pc) '())
  (set-actor-size! (pc) 'small)
  (define silent? #t)
  (add-sense-organ! (SenseOrgan 'chemical-gradient-detector 1 "chemical gradient detector") silent?)
  (add-manipulator! (Manipulator 'pseudopodia "cytoplasmic pseudopodia") silent?)
  (remove-manipulator! 'human-hands)
  (notice "OTAVA is a microscopic organism that evolves remarkably fast.")

  (create-task 'evolve)

  )

(define (on-begin-recurse-run)
  (current-recursion-depth (add1 (current-recursion-depth)))
  (dev-note "TODO:") ; TODO: randomize body plan

  ; (when (flag-set? 'scenario-evolution)
  ;   (on-begin-evolution))

  ; (cond [(not (flag-set? 'scenario-evolution))
  ;        (current-round 0)
  ;        (move-pc-to-location! (get-place-by-id 'perimeter))
  ;        @p{
  ;         Otava is at the perimeter of Martaanvuo wasteland. Her plan is a dumb fucking one for sure, but the bill has come due. "Fifteen days", he had said, "two weeks and a one day extra as an act of goodwill".

  ;         So, after getting rid of the bracelet (3 grams), Otava is now chasing the rumor of a [cache] of valuables in Martaanvuo wasteland, somewhere near the dam.
  ;        }
  ;        (create-task 'the-debt)
  ;        (wait-for-confirm)
  ;        (when (not (pc-has-sense-organ? 'eyes))
  ;          (p "There's a dim flat array of amorphous shapes. Fuzzy forms turn more solid. Otava begins to see.")
  ;          (add-sense-organ! (SenseOrgan 'eyes "eyes"))
  ;          (wait-for-confirm))
  ;        (when (pc-has-sense-organ? 'sonar)
  ;          (p "The afterimages of the shadowy outline of the world fade. Otava feels untethered.")
  ;          (remove-sense-organ! 'sonar)
  ;          (wait-for-confirm))
  ;        (when (pc-has-sense-organ? 'ears)
  ;          (p "Noise of the world fades into silence.")
  ;          (remove-sense-organ! 'ears)
  ;          (wait-for-confirm))
  ;         ]
  ;       )
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
  (cond
   [(flag-set? 'markbearer-killed)
    (notice "Otava having killed a markbearer, the deed carries over.")
    (add-to-play! (make-new-mb-pc))
    (set-markbearer-build!)
    (current-epithets (cons "Markbearer" (current-epithets)))
    (display-pc-title)
    (wait-for-confirm)
    ]
   [else
    (add-to-play! (make-new-pc))
    (set-base-build!)
    ])
  (when (not (= 1 (current-life)))
    (dev-note "Show life info"))

  (case (current-life)
    [(1)
     @p{
      Otava is at the perimeter of Martaanvuo wasteland. There's a rumor saying that upriver Martaanvuo, near the dam, there's this basement laboratory called the Maw, a fucking abandoned junkie cellar kitchen, and she'll find the [Book of Recombination] there.

      The Book of Recombination, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation. Find the book that will fill in the blanks! Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the clockwork of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh in the divine sausage machine, and out comes something magnificent:

      Otava the Seeker, become Otava the Deathless!
     }
     (current-epithets (cons "the Seeker" (current-epithets)))
     (create-task 'book-of-recombination)
     (wait-for-confirm)
     (advance-time-by-iotas! (d 6 12))
     (move-pc-to-location! (get-place-by-id 'perimeter))

     (p "Otava has two possible ways through Perimeter and into the Maw, the Shaman in her dreams has told her. Either the uphigh mountainwall rockypeak way thru Magpie hill, or throughdark tunnelblack underground way. The third way, the way-by-the-water, is inaccessible because of the dam.")

     (p "The Shaman in her dreams told her that there's evil brutes guarding the hill, Markbearers, twisted human-like creatures of endless malice and greed and hunger. To scale the hill, the Shaman had said, one must kill a Markbearer to show their worth.")

     (p "The tunnel entrance ought to be somewhere around here. Climbing Magpie hill looks arduous: The steep slope turns into an almost a vertical cliff.")

     (wait-for-confirm)

    ;  (notice "Some time passes.")
     #;(next-chapter!
       (string-append
         (take-random (list " – " ": " " "))
         (take-random (list "in which lessons are learned"
                            "in which a lesson is learned"
                            "a lesson is learned"
                            "some lessons are learned"
                            "lessons are learned"))))
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
