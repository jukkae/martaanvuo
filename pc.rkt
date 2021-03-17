#lang racket

(require "actions.rkt")
(require "actors.rkt")
(require "items.rkt")
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
    (field [current-statuses '()])

    (super-new)

    (define/public (get-current-defense)
      (cond ((memq 'parrying current-statuses)
             (add1 base-defense))
            (else base-defense)))

    (define/public (add-status! status)
      (set! current-statuses (cons status current-statuses)))
    (define/public (remove-status! status)
      (set! current-statuses status))
    (define/public (clear-statuses!)
      (set! current-statuses '()))

      
    (define/public (get-brawl-damage) (d 1 2))
    (define/public (get-next-command world) '())
    (define/public (get-generic-choices world)
      (define downtime-choices ; dumbass implementation
        (if (get-field in-combat world)
            '()
            (list
             (make-choice 'forage
                          "Forage."
                          (λ () (make-action #:symbol 'forage
                                             #:actor 'pc
                                             #:duration 100 ; 100 jiffies - half-a-day -> action economy: get better -> slightly better actions
                                             #:target null
                                             #:tags '(wilderness downtime))))
             (make-choice 'search
                          "Search."
                          (λ () (make-action #:symbol 'search
                                             #:actor 'pc
                                             #:duration 10
                                             #:target null
                                             #:tags '(wilderness downtime))))
             (make-choice 'search
                          "Craft."
                          (λ () (make-action #:symbol 'craft
                                             #:actor 'pc
                                             #:duration 50
                                             #:target null
                                             #:tags '(wilderness downtime)))))))
      (define free-choices
        (list (make-choice 'inventory
                           "Show inventory. [free action]"
                           (λ () (make-action #:symbol 'inventory
                                              #:actor 'pc
                                              #:duration 0
                                              #:target null
                                              #:tags '(always free))))))

      (append downtime-choices free-choices))
          
    
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