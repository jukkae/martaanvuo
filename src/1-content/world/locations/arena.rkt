#lang at-exp racket

(provide (all-defined-out))

; (require
;   "../../../0-engine/0-api/api.rkt")

; (add-place!
;  (place
;   #:id 'arena
;   #:type 'int
;   #:shortname "The Arena"
;   #:encounter-types '(voidfloater)
;   #:light-level 'dark
;   #:choices (list
;              (make-choice
;               'ring-new-enemy-bell
;               "Ring the bell"
;               `(
;                 (spawn-encounter)
;                 '()
;                 ))
;              (make-choice
;               'toggle-lights
;               "Toggle lights"
;               `(
;                 (case (get-current-light-level)
;                   ['bright
;                    (p "There's a click and the background hum is gone. And so is the light.")
;                    (set-location-light-level! (current-location) 'pitch-black)
;                    ]
;                   [else
;                    (p "There's an echoing clank. Spotlights turn on, aimed at the stage.")
;                    (set-location-light-level! (current-location) 'bright)
;                    ]
;                   )
;                 (notice (format "Light level is now ~a" (get-current-light-level)))
;               )
;               #:available-in-combat? #t
;               )
;              )
;   ))

; (add-route-between! 'martaanvuo-docks 'arena 5 'int #:light-level 'dark)
