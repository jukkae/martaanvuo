#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "info-card.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["martaanvuo.rkt"
  (engine-function
   pc
   paragraph
   in-combat?
   actor-in-range?
   set-in-combat?!
   move-actor-to-location!
   current-location
   )])


;;; MISC

(define *pending-action* '())
(define (reset-pending-action!)
  (set! *pending-action* '()))
(define (set-pending-action! action)
  (set! *pending-action* action))

(define (get-continue-pending-action-name pending-action)
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "Continue towards "
          (get-location-name-from-location-type (location-type (action-target pending-action)))
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "Keep on searching for paths."))
        (else (string-append "get-continue-pending-action-name: unknown action symbol: " (symbol->string (action-symbol pending-action))))))

;;;

(serializable-struct
 situation
 (world
  [pc #:mutable]
  [life #:mutable]
  [run #:mutable]
  [round #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [current-fragment #:mutable]
  [quests #:mutable]
  [grabberkin-encounters #:mutable]
  ))

(define *situation*
  (let ([new-world (world (list edgeflats swamp ridges valleys crematory ruins sewers cache workshop spring) 0 0)]
        [pc (make-new-pc)]
        [quests '()])
    (situation new-world pc 0 0 0 0 #f '() quests 0)))


(define (get-current-enemies)
  (filter
   (λ (actor) (and (actor-alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

(define (get-stance-range-numeric-value range)
  (case range
    ['engaged 0]
    ['close 1]
    [else (error "get-stance-range-numeric-value: unknown range")]))


(serializable-struct
 stance
 (index
  range
  location))


(define *enemy-stances* (make-hash))

(define (get-combatant-name actor)
  (cond ((pc-actor? actor)
         "Otava")
        (else
         (define stance (hash-ref! *enemy-stances* actor '()))
         (cond ((= (hash-count *enemy-stances*) 1)
                (append-string (actor-name actor)))
               (else
                (define name (actor-name actor))
                (define index (stance-index stance))
                (append-string name " " index))))))

(define (display-non-pc-combatant-info actor)
  (define stance (hash-ref! *enemy-stances* actor '()))
  (define name (get-combatant-name actor))
  (define hide-hp?
    (if (hash-ref (actor-traits actor) "hp-hidden" #f)
        #t
        #f))

  (define body
    (case (actor-name actor)
      [("Grabberkin")
       (list
        (list
         " HP "
         (if hide-hp?
             " ??? "
             (string-append " "
                            (number->string (actor-hp actor))
                            "/"
                            (number->string (actor-max-hp actor))
                            " "
                            ))))]
      [("Blindscraper")
       (list
        (list
         " size "
         (string-append " "
                        (get-trait actor "size")
                        " "
                        ))
        #;(list
           " location "
           (string-append " " (stance-location stance) " "))
        (list
         " range "
         (string-append " " (symbol->string (stance-range stance)) " "))

        )]))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (list " statuses " (~s statuses)))
    (set! body (append-element body statuses-list)))
  (info-card
   body
   name))

(define (engaged?)
  (define any-enemy-engaged? #f)
  (for ([(k stance) (in-hash *enemy-stances*)])
    (when (eq? (stance-range stance) 'engaged)
      (set! any-enemy-engaged? #t)))
  any-enemy-engaged?)


(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (hash-ref *enemy-stances* enemy '()))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

(define (in-range? target attack-mode)
  (case attack-mode
    ['melee #t]
    [else (displayln "in-range? not implemented yet for this attack mode")]))

(define (display-pc-combatant-info actor)
  (define name (get-combatant-name actor))
  (define body
    (list
     (list
      " HP "
      (string-append " "
                     (number->string (actor-hp actor))
                     "/"
                     (number->string (actor-max-hp actor))
                     " "
                     ))))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (list " statuses "
            (string-append " " (~s statuses) " ")))
    (set! body (append-element body statuses-list)))
  (info-card
   body
   name))

(define (display-combatant-info actor)
  (if (pc-actor? actor)
      (display-pc-combatant-info actor)
      (display-non-pc-combatant-info actor)))

(define (describe-combat-situation)
  (paragraph "Otava is in combat.")
  (for ([enemy (get-current-enemies)])
    (display-combatant-info enemy)
    
    )
  (display-pc-combatant-info (situation-pc *situation*))
  )

(define (serialize-state)
  ; prng can be stored as vector:
  ; https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._pseudo-random-generator-~3evector%29%29
  '())

(define (serialize-input)
  '())



(define (describe-situation)
  (cond
    ((in-combat?) (describe-combat-situation)))
  )

(define (redescribe-situation)
  (cond
    ((in-combat?) (describe-combat-situation))
    (else (displayln "redescribe-situation: TODO")))
  )