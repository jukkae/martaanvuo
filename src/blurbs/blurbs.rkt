#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../core/io.rkt"
  "../core/utils.rkt")

(lazy-require
 ("../state/state.rkt" [current-times-species-encountered
                        current-times-species-encountered++]))

(define (blurb name)
  (case name
    ['begin-first-run
 (p @~a{
  Otava is following an old, overgrown path through the foggy woods. Late-morning gloom, forest like monochrome cardboard. Air thick with the damp smell of old, decaying forest.

  This should be worth it: There's a [cache] of valuables deep in the forest, but somewhere near it there's this basement lab too, a fucking abandoned junkie cellar kitchen, and if what she knows and what she's figured out is correct, she'll find the [Anthead Monograph] there.

  The first should net enough gold for Otava to pay back Mediator her debt anyway. That alone would be good enough a reason.

  The second one, though, Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation, and then none of this will matter anyway. The last step is to find the book that will fill in the blanks and make it all make sense. Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh into the divine sausage machine, and out comes something magnificent:

  Otava the Seeker, become Otava the Deathless!
  })
 ]))

(define (next-blurb name)
  (case name
    ['ants
     (current-times-species-encountered++ 'ants)

     (case (hash-ref (current-times-species-encountered) 'ants)
       [(1)
        (p @~a{
              A small squad of ants is marching down the narrow, barely noticeable right-hand path that's sloping down towards Martaanvuo swamp.
              })
        ]
       [(2)
        (p @~a{
              The ant squad that started on the right-hand path towards the swamp is now outside all paths and making their own. The squad has encountered a fresh carrion of a vast centipede and is chopping it up.
              })
        ]
       [(3)
        (p @~a{
              The ant squad is smaller than before. One of the ants was found infected by the mind-control fungus, and was promptly executed.
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
