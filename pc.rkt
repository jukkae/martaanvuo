#lang racket

(require "action.rkt")
(require "actor.rkt")
(require "items.rkt")
(require "skills.rkt")
(require "ui.rkt")
(require "utils.rkt")
; see also lazy-require and units

(define pc%
  (class* object% (actor<%>)
    (field [max-hp 4])
    (field [hp 4])
    (field [base-defense 6])
    (field [attack-skill 1])
    (field [inventory '()])
    (field [last-breaths-amount 1]) ; require time to pass in game world, do not measure turns or locations -> different "layer" of abstraction
    (field [statuses '()]) ; combat
    (field [conditions '()]) ; non-combat
    (field [hunger-counter 0])

    (field [current-location '()]) ; convenience

    (super-new)

    (define/public (get-current-defense)
      (cond ((memq 'parrying statuses)
             (add1 base-defense))
            (else base-defense)))

    (define/public (add-status! status)
      (set! statuses (cons status statuses)))
    (define/public (remove-status! status)
      (set! statuses (remove status statuses)))
    (define/public (clear-statuses!)
      (set! statuses '()))

    (define/public (add-condition! condition)
      (set! conditions (cons condition conditions)))
    (define/public (remove-condition! condition)
      (set! conditions (remove condition conditions)))
    (define/public (clear-conditions!)
      (set! conditions '()))

    (define/public (advance-time-by-a-jiffy!)
      (define previous-hunger hunger-counter)
      (set! hunger-counter (add1 hunger-counter))
      (when (and
             (< previous-hunger 400)
             (>= hunger-counter 400))
        (displayln "You are hungry.") ; TODO confirm this
        (wait-for-confirm)
        (add-condition! 'hungry))
      (when (and
             (< previous-hunger 800)
             (>= hunger-counter 800))
        (displayln "You are really hungry.")
        (wait-for-confirm))
      (when (and
             (< previous-hunger 1200)
             (>= hunger-counter 1200))
        (displayln "You are starving.")
        (set! hp (- hp 1))
        (wait-for-confirm))) ; TODO use (hit) or something similar instead of just directly mutating the value

      
    (define/public (get-brawl-damage) (d 1 2))
    (define/public (get-next-command world) '())

    (define/public (get-generic-choices world)
      (get-choices world this))
          
    
    (define/public (get-combat-choices world)
      (define targets (send world get-current-enemies))
      (define combat-choices '())
      (if (get-field in-combat world)
          (begin
            (for ([i (in-range 0 (length targets))])
              (define target (list-ref targets i))
              (set! combat-choices
                    (append combat-choices
                            (list (make-choice 'brawl
                                               (string-append "Attack the " (send target get-name) " (enemy #" (number->string (add1 i)) ")")
                                               (λ () (make-action #:symbol 'brawl
                                                                  #:actor 'pc
                                                                  #:duration 1
                                                                  #:target target
                                                                  #:tags '(combat fast delayed-resolution)))))))
              )
            (set! combat-choices
                  (append combat-choices
                          (list (make-choice 'parry
                                             (string-append "Wait for an opening.")
                                             (λ () (begin
                                                     (add-status! 'parrying)
                                                     (make-action #:symbol 'defensive-strike
                                                                  #:actor 'pc
                                                                  #:duration 1
                                                                  #:target 'random
                                                                  #:tags '(combat slow delayed-resolution))))))))
            (set! combat-choices
                  (append combat-choices
                          (list (make-choice 'run
                                             "Run."
                                             (λ () (make-action #:symbol 'run
                                                                #:actor 'pc
                                                                #:duration 1
                                                                #:target null
                                                                #:tags '(combat))))))))
          '())
      combat-choices)
    
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (if (= last-breaths-amount 0)
                     (begin (set! hp 0)
                            'u-ded)
                     (begin (set! hp 0)
                            (set! last-breaths-amount 0)
                            'last-breath))
                 'u-hit)))

    (define/public (get-name) "You")
    (define/public (get-a-hunch)
      (take-random '(
                     "You feel something is watching you.")))
    (define/public (get-status)
      (cond ((> hp 2) "")
            ((= hp 2) "You have broken bones, but you're still holding on.")
            ((= hp 1) "You are badly hurt, but still breathing.")
            ((= hp 0) "It's a miracle you're still alive.")
            ((< hp 0) "There is nothing but the Dark. A thing that once was alive is no more.")))))


(define *actors* (list (new pc%)))


(provide (all-defined-out))