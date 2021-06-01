#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "io.rkt")
(require "location.rkt")
(require "situation.rkt")
(require "stance.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["martaanvuo.rkt"
  (actor-in-range?
   move-actor-to-location!
   )])

(define (get-nighttime-choices world actor)
  (displayln "get-night-time-choices: TODO not implemented yet")
  (list
   (make-choice
    'sleep
    "Sleep." 
    (λ () (make-action
           #:symbol 'sleep
           #:actor (pc)
           #:duration 100
           #:target '()
           #:tags '()
           #:details '())))))

(define (get-world-choices world actor)
  (cond ((in-combat?)
         (get-combat-choices world actor))
        ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
         (get-nighttime-choices world actor))
        (else (get-downtime-choices world actor))))

(define (get-combat-choices world actor)
  (define targets (get-current-enemies))

  (define combat-choices '())

  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (define stance (hash-ref (situation-enemy-stances *situation*) target))
    (cond ((or (eq? (stance-range stance) 'close)
               (eq? (stance-range stance) 'engaged))
           (define damage-roll (λ () (d 1 2)))
           (define details
             (list
              (cons 'damage-roll damage-roll)
              (cons 'damage-roll-formula "1d2")
              (cons 'damage-type 'bludgeoning)
              ))
           (define choice
             (make-choice
              'attack
              (string-append
               "Attack "
               (get-combatant-name target)
               " in melee with crowbar.")
              (λ ()
                (make-action
                 #:symbol 'melee
                 #:actor (situation-pc *situation*)
                 #:duration 1
                 #:target target
                 #:tags '(initiative-based-resolution)
                 #:details details))))
           (set! combat-choices (append-element combat-choices choice)))
          ))

  (cond ((not (engaged?))
         (define run-choice
           (make-choice
            'flee
            (string-append
             "Run.")
            (λ ()
              (make-action
               #:symbol 'flee
               #:actor (situation-pc *situation*)
               #:duration 1
               #:target '()
               #:tags '(initiative-based-resolution fast)
               #:details '()))))
         (set! combat-choices (append-element combat-choices run-choice))))

  (define engaged-enemies (get-enemies-at-range 'engaged))
  (define engaged-grabberkin
    (filter (λ (enemy) (equal? (actor-name (get-an-enemy-at-range 'engaged))
                               "Grabberkin"))
            engaged-enemies))

  (cond ((not (null? engaged-grabberkin))

         ; TODO shit action
         (define strength-mod (get-attribute-modifier-for (actor-strength actor)))
         (define damage-roll (λ () (d 1 2)))
         (define details
           (list
            (cons 'damage-roll damage-roll)
            (cons 'damage-roll-formula
                  (string-append "1d2 + str mod ["
                                 (number->string strength-mod)
                                 "]"))
            (cons 'damage-type 'bludgeoning)
            ))
         (define break-free-choice
           (make-choice
            'wrestle
            (string-append
             "Try to pull the leg free.")
            (λ ()
              (make-action
               #:symbol 'wrestle
               #:actor (situation-pc *situation*)
               #:duration 1
               #:target (get-an-enemy-at-range 'engaged)
               #:tags '(initiative-based-resolution)
               #:details details))))
         (set! combat-choices (append-element combat-choices break-free-choice))))
 

  combat-choices
  )

; This should be refactored
; - a big question is, where does much of this logic
; best fit? locations?
; actions.rkt, as in "the grand action table containing possible actions"?
(define (get-downtime-choices world actor)
  (define pending-choices '())
  (when (not (null? (situation-pending-action *situation*)))
    (set!
     pending-choices
     (list
      (make-choice
       'go-to-location
       (get-continue-pending-action-name)
       (λ ()
         (begin0
           (situation-pending-action *situation*)
           (reset-pending-action!)))))))
  ;(displayln "PC-not-null")

  
  (define change-location-choices '())
  (define downtime-choices '())
  (when (and (not (in-combat?))
             (not (location-has-tag? (current-location) 'forbid-simple-exit)))
    (cond ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
           '()))
    (define neighbors
      (location-neighbors (current-location)))
    (for ([i (in-range 0 (length neighbors))])
      (define neighbor (list-ref neighbors i))
      (set! change-location-choices
            (append change-location-choices
                    (list
                     (make-choice
                      'go-to-location
                      (get-go-to-text-from-location-to-another (location-type (current-location)) (location-type neighbor)) 
                      (λ () (make-action
                             #:symbol 'go-to-location
                             #:actor (situation-pc *situation*)
                             #:duration 100
                             #:target neighbor
                             #:tags '(downtime)
                             #:details '())))))))

    (set! downtime-choices
          (if (eq? (location-type (current-location)) 'swamp)
              (list
               (make-choice
                'forage
                (string-append "Forage.")
                (λ () (make-action
                       #:symbol 'forage
                       #:actor (situation-pc *situation*)
                       #:duration 100
                       #:target '()
                       #:tags '(downtime)
                       #:details '()))))
              '())


          ))

  (define end-run-choices '()) ; poor name
  (when (eq? (location-type (current-location)) 'edgeflats)
    (set! end-run-choices
          (list
           (make-choice
            'go-back-to-the-shack
            "Head back to The Shack."
            (λ () (make-action
                   #:symbol 'end-run
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)
                   #:details '()))))))
  (when (eq? (location-type (current-location)) 'spring)
    (set! end-run-choices
          (list
           (make-choice
            'dive-in-spring
            "Dive in the spring."
            (λ () (make-action
                   #:symbol 'win-game
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)
                   #:details '()))))))

  
  (define neighbors
    (location-neighbors (current-location)))

  (define location-specific-choices
    (for/list ([action (location-actions-provided (current-location))])
      (case action
        ['search-for-paths
         (make-choice
          'search-for-paths
          "Search for paths."
          (λ () (make-action
                 #:symbol 'search-for-paths
                 #:actor (situation-pc *situation*)
                 #:duration 100
                 #:target '()
                 #:tags '(downtime)
                 #:details '())))])))
  (define choices-before-pruning
    (append pending-choices change-location-choices downtime-choices end-run-choices location-specific-choices))

  (define (show-choice-based-on-pending-choice? choice)
    (cond ((not (null? pending-choices))
           #f)
          (else
           #t)))
  
  (define
    pruned-choices
    (filter
     show-choice-based-on-pending-choice?
     choices-before-pruning))
  (define all-choices
    (append pending-choices pruned-choices))
  all-choices)


; store in the action, handle calling from here
; -> code to action handler?
(define (describe-pc-intention pc-action)
  (case (action-symbol pc-action)
    ['forage (paragraph "Otava is getting low on supplies. Too low to be comfortable. Here looks good as any, so she decides to take a look around, see if there's anything edible.")]
    #;[else (paragraph "TBD")]))