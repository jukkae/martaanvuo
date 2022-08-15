#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(add-place!
  (place
    #:id 'forge
    #:shortname "Forge of Master Seppo"
    #:choices
    (list
     (choice
          'buy-mods
          "Swap sensory outfit"
          `(
            (go-to-fragment 'seppo)
            '()
            ))
    )))

(add-route-between! 'perimeter 'forge 1)
