#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action-resolver.rkt")
(require "action.rkt")
(require "actor.rkt")
(require "io.rkt")
(require "situation.rkt")
(require "status.rkt")
(require "utils.rkt")
(require "world.rkt")


(define (make-grabberkin-action actor action-flag)
  (case action-flag
    ['pull-under
     (make-action
      #:symbol 'pull-under
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['anklebreaker
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'anklebreaker
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]
    
    ['tighten-grip
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'tighten-grip
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]
    
    ['skip
     (make-action
      #:symbol 'skip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(silent))]

    ['grab
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 0
      #:target (pc)
      #:tags '(initiative-based-resolution fast)
      #:details (list (status 'bound 3)))]

    ['release-grip
     (make-action
      #:symbol 'release-grip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(fast))]

    [else
     (error (string-append
             "make-grabberkin-action: unknown action: "
             (symbol->string action-flag)))]))

(define (get-grabberkin-action actor)
  (cond ((in-combat?)
         (cond
           ((>= (actor-hp actor) 8)

            (cond
              ((and (actor-in-range? actor 'engaged)
                    (actor-has-status-of-type? (pc) 'bound))
               (define options
                 (list
                  (cons 1 'pull-under)
                  (cons 2 'anklebreaker)
                  (cons 3 'tighten-grip)
                  (cons 4 'skip)))
               ;(define roll (d 1 4))
               (define roll 1)
               (define index (- roll 1))
               (define action-flag-with-index (list-ref options index))

               
               (define action-flag (cdr action-flag-with-index))
               (make-grabberkin-action actor action-flag))
              (else
               (make-grabberkin-action actor 'grab)
               )))
           
           ((< (actor-hp actor) 8)
            (make-grabberkin-action actor 'release-grip))))
        (else
         (begin (displayln "Grabberkin AI, not in combat")))))

(define (spawn-grabberkin-encounter!)
  ; TODO usually grab only one ankle, sometimes both
  (paragraph "Otava feels something like a hand grab her ankle.")
  (set-in-combat?! #t)

  (define i 0)
  (define enemy (make-actor "Grabberkin" 14))
  (set-actor-dexterity! enemy 4)
  (set-actor-strength! enemy 11)
  (set-trait! enemy "defense" -1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "hp-hidden" #t)
  (move-actor-to-location! enemy (current-location))

  (inflict-status! (pc) (status 'bound 3))

  (define index
    (case i
      [(0) "α"]
      [(1) "β"]))
  (define range 'engaged)
  (define location "grabbing Otava's ankle")
  (define enemy-stance
    (stance index range location))
           
  (hash-set! *enemy-stances* enemy enemy-stance)
  )