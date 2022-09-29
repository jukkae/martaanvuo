#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(require
  "carnival.rkt"
  "perimeter.rkt"
  )

(define (get-precipice-choices)
  (make-choice
    'fall-down
    "Check out the mesmerising overhang."
    (λ ()
      (p "The glowing fog is starting to coalesce around her, as a rock gets loose under her foot. Otava slips.")
      (go-to-fragment 'fall-down)
      (wait-for-confirm)
      'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round
  ; TODO: - look into the abyss (+xp, chance of falling) - jump
  )

(define (get-location-choices location)
  (case (location-id location)
   ['perimeter (get-perimeter-choices)]
   ['luminous-precipice (get-precipice-choices)]
   ['carnival (get-carnival-choices)]
   [else

    (filter
   (λ (x) (and (not (null? x))
               (not (void? x))))
   (for/list ([feature (location-features (current-location))])
     (case feature

       ['waiting-room-begin
        (make-choice
         'waiting-room
         "Enter the waiting room."
         (λ ()
           (p "The penultimate step towards Ascending to a Higher Plane of Existence: To enter the waiting room!")
           (go-to-fragment 'waiting-room-1)
           'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round
        ]
        ['anthill
         (cond [(not (flag-set? 'anthill-seen))
          (make-choice
          'anthill
          "Anthill."
          (λ ()
            (set-flag 'anthill-seen)
            (go-to-fragment 'anthill-1)
            'end-chapter ; ie., 'end-round-early, plus next chapter on next round
            ))]
           [else
            (define next-anthill-fragment
            (cond [(pc-has-item? 'grabberkin-finger)
                  'anthill-complete-fingers]
                  [else 'anthill-2]))

            (make-choice
            'anthill
            "Back to Anthill."
            (λ ()
              (go-to-fragment next-anthill-fragment)
              'end-round-early
              ))])]

       #;[else (dev-note (format "unknown feature ~a" feature))])))

    ]))
