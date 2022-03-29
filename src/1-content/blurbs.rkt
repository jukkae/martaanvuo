#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../0-engine/2-core/io.rkt"
  "../0-engine/2-core/core.rkt")

(lazy-require ("../0-engine/7-state/state/state.rkt"
  [current-times-species-encountered
   current-times-species-encountered++
   ]))

(define (get-blurb name)
  (case name
    ['the-end
     @~a{
      The end.



      M A R T A A N V U O
      ===================

      Jukka Eerikäinen (2022)


      }]
    ['begin-first-run-pt-1

 @~a{
  Otava wakes up from her thoughts. She is following an old, overgrown path through the foggy woods. Late-morning gloom, forest like monochrome cardboard. Air thick with the damp smell of old, decaying forest. Easy going so far, she just let her mind wander a bit, to think on the spoils.

  This should be worth it: There's a [cache] of valuables deep in the forest, but somewhere near it there's this basement lab too, a fucking abandoned junkie cellar kitchen, and if what she knows and what she's figured out is correct, she'll find the [Anthead Monograph] there.

  The first one: Should be enough gold for Otava to pay back Mediator her debt anyway. That alone would be good enough a reason.
 }
 ] ; subsequent runs: if reached subtarget X, then blurb Y, otherwise blurb Z
    ['begin-first-run-pt-2

 @~a{
  The second one, though, the Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation. Find the book that will fill in the blanks. Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh into the divine sausage machine, and out comes something magnificent:

  Otava the Seeker, become Otava the Deathless!
 }
 ]

    ['martaanvuo-title
     @~a{
      M A R T A A N V U O
      ===================
      }]

    ; idea: this (and other similar semi-random content stuff) could also either use p or notice, depending on situation
    ['rest-action
     (define str @~a{
      Time passes.
      Some time passes.
      A while passes.
     })
     (define l (string-split str #rx"\n+"))
     (take-random l)
     ]

    [else (dev-note (format "Unknown blurb: ~a" name))
     '()]

    ))

(define (blurb name)
  (p (get-blurb name))
  )

(define (next-blurb name)
  (case name
    ['ants
     (current-times-species-encountered++ 'ants)

     (case (hash-ref (current-times-species-encountered) 'ants)
       [(1)
        (p @~a{
              A small squad of ants is marching down the narrow, barely noticeable right-hand path that's sloping down towards Martaanvuo swamp. This is as far as the colony has ever known, and they are the first ants to go beyond. Slowly increasing smell of rotten flesh: Novel sensation, but soon deemed to be of no importance.
              })
        ]
       [(2)
        (p @~a{
              The ant squad got infected by the mind-control fungus and made a mass suicide somewhere along the path towards Martaanvuo swamp.
              })
        ]
       [(3)
        (p @~a{
              A small squad of ants gets to the Graveyard of the Mind-Controlled Slaves somewhere along the sloping path towards Martaanvuo, the Rotten-Flesh Path, the one that has to be walked.
              })
        ]
       [(4)
        (p @~a{
              The ant squad, unaware of their impending obsoletion, has sent one back as a messenger. It gets to the bridge of the medium-sized pebble, where it gets eaten by a sparrow.
              })
        ]
       [else
        '()]
       )
     ]))
