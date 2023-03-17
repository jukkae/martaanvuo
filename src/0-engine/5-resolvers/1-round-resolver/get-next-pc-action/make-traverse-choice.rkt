#lang at-exp racket

(require "../../../1-index/content.rkt"

         "../../../3-types/action.rkt"
         "../../../3-types/choice.rkt"
         "../../../3-types/location.rkt"
         "../../../3-types/place.rkt"
         "../../../3-types/route.rkt"

         "../../../4-systems/actors/conditions.rkt"
         "../../../4-systems/world/world.rkt"

         "../../../7-state/state.rkt")

(provide make-traverse-choice)
(define (make-traverse-choice route direction)
  (define traverse-duration (route-traverse-time route))
  (define duration-reason "")
  (when (actor-has-condition-of-type? (pc) 'ankle-broken)
    (set! traverse-duration (* 3 traverse-duration))
    (set! duration-reason " – ankle broken (3x)"))
  (make-choice
   'traverse
   (format "~a [~a ι~a]"
           (get-traverse-text route (current-location))
           traverse-duration
           duration-reason)
   (λ ()
     (make-action
      #:symbol 'traverse
      #:actor (pc)
      #:duration traverse-duration
      #:target (location-id route)
      #:tags '(downtime)
      #:details (list direction)
      #:on-before-rules
      `((let/ec
         return
         (describe-begin-traverse-action (get-route-by-id ',(location-id route)) ',direction)
         (define en-route-location (get-route-by-id ',(location-id route)))
         (move-pc-to-location! en-route-location)
         (when (not (null? (location-encounter-types (current-location))))
           (define duration (exact-floor (/ ,traverse-duration 2)))

           (define tl (advance-time-until-next-interesting-event! duration #f))
           (define events (timeline-events tl))

           (cond
             [(timeline-interrupted? tl)
              return
              tl])

           ; now halfway through
           (define bonus -2)
           (define bonus-str "-2")
           (define encounter-roll (+ (d 1 6) bonus))
           (define tn 3)
           (notice (format "~a Encounter roll: 1d6~a >= ~a: [~a] – ~a"
                           (timestamp)
                           bonus-str
                           tn
                           encounter-roll
                           (if (< encounter-roll tn) "fail" "success")))

           ; fail encounter check
           (when (< encounter-roll tn)
             (define resolve-events
               (list (make-event ,''spawn-encounter
                                 '() ; pack info about enemies / event here
                                 #:interrupting? #t)))
             (define metadata '(interrupted))

             (set-timeline-events! tl (append (timeline-events tl) resolve-events))
             (set-timeline-metadata! tl (append (timeline-metadata tl) metadata))

             (process-timeline! tl)
             (return tl))

           ; encounter check successful

           (define second-world-tl (advance-time-until-next-interesting-event! duration #f))

           (set-timeline-events! tl (append (timeline-events tl) (timeline-events second-world-tl)))
           (set-timeline-metadata!
            tl
            (append (timeline-metadata tl) (timeline-metadata second-world-tl)))
           (set-timeline-duration! tl (+ (timeline-duration tl) (timeline-duration second-world-tl)))

           (return tl))
         'before-action-ok))
      #:resolution-rules
      `((set-route-traversed! (get-route-by-id ',(location-id route)))
        (define next-location-id ',(if (equal? direction 'a-to-b) (route-b route) (route-a route)))
        (define next-location (get-location-by-id next-location-id))
        #;(define tl (advance-time-until-next-interesting-event! ,traverse-duration #f))
        (define tl (timeline '() '() 0))
        (process-timeline! tl)
        (cond
          #;(notice (format "~a TIMELINE INTERRUPTED" (timestamp)))
          [(timeline-interrupted? tl) 'time-passing-handled]
          [else
           (move-pc-to-location! next-location)
           (set! tl 'time-passing-handled) ; TODO: HACKY!!
           ])
        tl)
      #:on-after-rules `((describe-finish-traverse-action (get-route-by-id ',(location-id route))
                                                          ',direction))))))
