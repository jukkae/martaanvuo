#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(define (display-statusline)
  (define current-day (add1 (exact-floor (/ (world-elapsed-time (current-world)) day-length))))
  (if (not (equal? (location-id (current-location)) 'waiting-room))
      (notice (format "~a ~a~a, day ~a, ~a [~a ι].~a"
                      (timestamp)
                      (if (current-in-combat?) "[In combat] " "")
                      (get-location-short-description (current-location))
                      current-day
                      (symbol->string (time-of-day-from-iotas (world-elapsed-time (current-world))))
                      (remainder (world-elapsed-time (current-world)) day-length)
                      (if (>= (pc-actor-hunger (pc)) hunger-level-hungry)
                          (format " Hunger: ~a."
                                  (case (pc-hunger-level)
                                    ['satiated "satiated"]
                                    ['not-hungry "not hungry"]
                                    ['hungry "hungry"]
                                    ['very-hungry "very hungry"]
                                    ['starving "starving"]))
                          "")))
      (notice (format "Waiting room.")))

  (notice (format "~a~a~a"
                  (cond
                    [(pc-has-sense-organ? 'eyes) (format "light level: ~a" (get-current-light-level))]
                    [else ""])
                  (cond
                    [(and (pc-has-sense-organ? 'eyes)
                          (or (pc-has-sense-organ? 'ears) (pc-has-sense-organ? 'sonar)))
                     ", "]
                    [else ""])
                  (cond
                    [(or (pc-has-sense-organ? 'ears) (pc-has-sense-organ? 'sonar))
                     (format "noise level: ~a" (get-current-noise-level))]
                    [else ""])))

  (cond
    [(and (Place? (current-location)))
     (define exploration-string
       (case (Place-explored (current-location))
         ['not-explored "not explored"]
         ['() "not explored"]
         ['partially-explored "partially explored"]
         ['explored "explored"]
         ['exhaustively-explored "exhaustively explored"]
         [else (format "~a" (Place-explored (current-location)))]))
     (notice (format "The place is ~a." exploration-string))])

  (when (not (empty? (location-items (current-location))))
    (define items (location-items (current-location)))
    (case (length items)
      [(1)
       (define item (car items))
       (define name
         (cond
           [(item? item) (item-name item)]
           [else (format "~a" item)]))
       (notice (format "There is ~a here."
                       (cond
                         [(Name? name)
                          (format "~a ~a" (Name-indefinite-article name) (Name-singular name))]
                         [(string? name) (string-downcase (format "~a" name))]
                         [else
                          (displayln name)
                          (error "Unknown type for name!")])))]
      [else (notice "There are multiple items here.")])))
