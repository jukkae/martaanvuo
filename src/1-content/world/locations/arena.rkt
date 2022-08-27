#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(add-place!
 (place
  #:id 'arena
  #:type 'int
  #:shortname "The Arena"
  #:encounter-types '(voidfloater)
  #:light-level 'dark
  #:choices (list
             (make-choice
              'ring-new-enemy-bell
              "Ring the bell"
              `(
                (spawn-encounter)
                '()
                ))
             (make-choice
              'toggle-lights
              "Toggle lights"
              `(
                (case (get-current-light-level)
                  ['bright
                   (set-location-light-level! (current-location) 'pitch-black)
                   ]
                  [else
                   (set-location-light-level! (current-location) 'bright)
                   ]
                  )
              )
              #:available-in-combat? #t
              )
             )
  ))

(add-route-between! 'martaanvuo-docks 'arena 5 'int #:light-level 'dark)
