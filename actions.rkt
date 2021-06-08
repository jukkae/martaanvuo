#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require rebellion/collection/association-list)

(require "action.rkt")
(require "actor.rkt")
(require "choice.rkt")
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
           #:duration 100)))))

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
           (define damage-roll (λ () (d 1 6)))
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
               #:tags '(initiative-based-resolution fast)))))
         (set! combat-choices (append-element combat-choices run-choice))))

  (define engaged-enemies (get-enemies-at-range 'engaged))
  (define engaged-grabberkin
    (filter (λ (enemy) (equal? (actor-name (get-an-enemy-at-range 'engaged))
                               "Grabberkin"))
            engaged-enemies))

  (cond ((not (null? engaged-grabberkin))

         (define strength-mod (get-attribute-modifier-for (actor-strength actor)))
         
         (define details
           (association-list 'str-mod strength-mod))
         
         (define break-free-choice
           (make-choice
            'pull-free
            (string-append
             "Try to pull the leg free.")
            (λ ()
              (make-action
               #:symbol 'break-free
               #:actor (situation-pc *situation*)
               #:duration 1
               #:target (take-random engaged-grabberkin)
               #:tags '(initiative-based-resolution fast)
               #:details details))))
         (set! combat-choices (append-element combat-choices break-free-choice))))
 

  combat-choices
  )


(define (get-downtime-choices world actor)
  (define (show-based-on-pending-choice? choice)
    (if (null? (situation-pending-action *situation*))
        #t
        (begin
          (cond
            ; show pending action
            ((string-prefix? (choice-name choice) "[continue]")
             #t)
            
            ; don't show actions that have same symbol as pending action
            ; (note: this may or may not lead to intended results, see how it works)
            ; plot twist: it is shit and has to be fixed
            ((eq? (choice-symbol choice) (action-symbol (situation-pending-action *situation*)))
             #f)
            
            ; show anything else
            (else
             #t)))))
  

  (flatten
   (filter ; okay now this is getting ridiculous
    (λ (x) (show-based-on-pending-choice? x))
    (flatten ; CBA
     (filter ; TODO this should be extracted, useful, esp. the void check!
      (λ (x) (and (not (null? x))
                  (not (void? x))))
      (list
    
       (when (not (null? (situation-pending-action *situation*)))
         (choice
          (action-symbol (situation-pending-action *situation*))
          (get-continue-pending-action-name)
     
          (λ ()
            (begin0
              (situation-pending-action *situation*)
              (reset-pending-action!)))))

       (when (and (not (in-combat?))
                  (not (location-has-tag? (current-location) 'forbid-simple-exit)))
         (cond ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
                '()))
      
         (for/list ([neighbor (location-neighbors (current-location))])
        
           (make-choice
            'go-to-location
            (get-go-to-text-from-location-to-another (location-type (current-location)) (location-type neighbor)) 
            (λ () (make-action
                   #:symbol 'go-to-location
                   #:actor (situation-pc *situation*)
                   #:duration 100
                   #:target neighbor
                   #:tags '(downtime))))))

       (when (eq? (location-type (current-location)) 'swamp)
         (list
          (make-choice
           'forage
           (string-append "Forage.")
           (λ () (make-action
                  #:symbol 'forage
                  #:actor (situation-pc *situation*)
                  #:duration 100
                  #:tags '(downtime))))))

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
                    #:tags '(downtime))))]))

       (when (eq? (location-type (current-location)) 'spring)
         (make-choice
          'dive-in-spring
          "Dive in the spring."
          (λ () (make-action
                 #:symbol 'win-game
                 #:actor (situation-pc *situation*)
                 #:duration 0
                 #:tags '(downtime)))))
       
       (when (eq? (location-type (current-location)) 'edgeflats)
         (make-pc-choice
          #:id 'end-run
          #:text "Head back to The Shack."
          #:duration 0
          #:tags '(downtime)))

       )))))
  )
    



; store in the action, handle calling from here
; -> code to action handler?
(define (describe-pc-intention pc-action)
  (case (action-symbol pc-action)
    ['forage (paragraph "Otava is getting low on supplies. Too low to be comfortable. Here looks good as any, so she decides to take a look around, see if there's anything edible.")]
    #;[else (paragraph "TBD")]))