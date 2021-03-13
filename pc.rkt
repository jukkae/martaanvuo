#lang racket

(require "actions.rkt")
(require "actors.rkt")
(require "items.rkt")
(require "utils.rkt")
; see also lazy-require and units

(define pc%
  (class* object% (actor<%>)
    (field [hp 4])
    (field [attack-skill 1])
    (field [inventory (list (new twine%))])

    (super-new)

    (define/public (get-brawl-damage) (d 1 2))
    (define/public (get-next-command world) '())
    (define/public (get-generic-choices world)
      (if (not (empty? (get-field inventory this)))
          (list (make-choice 'inventory
                             "Show inventory. [free action]"
                             (λ () (make-action #:symbol 'inventory
                                                #:actor 'pc
                                                #:duration 0
                                                #:target null
                                                #:tags '(always free)))))
          '()))
    
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
                                                                  #:tags '(combat fast delayed-resolution))))))))
            (set! combat-choices
                  (append combat-choices
                          (list (make-choice 'parry
                                             "Wait for an opening, then strike."
                                             (λ () (begin
                                                     (displayln "Should set temporary defense bonus!")
                                                     (make-action #:symbol 'parry
                                                                  #:actor 'pc
                                                                  #:duration 1
                                                                  #:target null
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
                 (begin (set! hp 0)
                        'u-ded)
                 'u-hit)))

    (define/public (get-name) "You")
    (define/public (get-a-hunch)
      (take-random '(
                     "You feel something is watching you.")))
    (define/public (get-status)
      (cond ((> hp 2) "")
            ((= hp 2) "You have broken bones, but you're still holding on.")
            ((= hp 1) "You are badly hurt, but still breathing. Barely.")
            ((= hp 0) "There is nothing but void. A thing that once was alive is no more.")))))


(define *actors* (list (new pc%)))


(provide (all-defined-out))