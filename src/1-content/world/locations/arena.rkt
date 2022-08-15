#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(add-place!
  (place
    #:id 'arena
    #:type 'outdoors
    #:shortname "The Arena"
    #:encounter-types '(voidfloater blindscraper)
    #:choices
    (list
     (choice
          'ring-new-enemy-bell
          "Ring the Bell"
          `(
            (spawn-encounter)
            '()
            ))
    )))

(add-route-between! 'martaanvuo-docks 'arena 5)
