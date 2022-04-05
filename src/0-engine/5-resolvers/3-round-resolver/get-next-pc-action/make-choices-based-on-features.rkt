#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../2-core/io.rkt"
  "../../../2-core/core.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/route.rkt"

  "../../../4-systems/actors/actor.rkt"
  "../../../4-systems/blurbs/blurbs.rkt"
  "../../../4-systems/checks/checks.rkt"
  "../../../4-systems/items/item.rkt"
  "../../../4-systems/locations/locations.rkt"
  "../../../4-systems/pc/pc.rkt"
  "../../../4-systems/world/time.rkt"
  "../../../4-systems/world/world.rkt"

  "../../../7-state/state.rkt"
  )

(require racket/lazy-require)

(lazy-require ["../round-resolver.rkt"
               (go-to-fragment
                )])

(provide make-choices-based-on-features)
(define (make-choices-based-on-features)
  (filter
   (λ (x) (and (not (null? x))
               (not (void? x))))
   (for/list ([feature (location-features (current-location))])
     (case feature
       ['magpie-effigy
        (make-choice
         'follow-the-magpie
         "Magpie."
         (λ ()
           (p "Despite the worsening rain, Otava goes into the monochrome bush.")
           (go-to-fragment 'magpie)
           (remove-feature-from-location! (current-location) 'magpie-effigy)
           'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

        ]

       ['precipice
        (make-choice
         'fall-down
         "Check out the mesmerising overhang."
         (λ ()
           (p "The glowing fog is starting to coalesce around her, as a rock gets loose under her foot. Otava slips.")
           (go-to-fragment 'fall-down)
           (wait-for-confirm)
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

               ; TODO: This should be known and decided by in "content related to anthill" – for instance perhaps in fragments/anthill?
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
                  ))])
        ]

       ['waiting-room-begin
        (make-choice
         'waiting-room
         "Enter the waiting room."
         (λ ()
           (p "The penultimate step towards Ascending to a Higher Plane of Existence: To enter the waiting room!")
           (go-to-fragment 'waiting-room-1)
           'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

        ]

       [else (dev-note (format "unknown feature ~a" feature))]))))