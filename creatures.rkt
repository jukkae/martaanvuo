#lang racket

(require "action.rkt")
(require "utils.rkt")

(define creature<%> (interface () get-name get-name-with-article get-description get-next-action get-status hit))
(define bloodleech%
  (class* object% (creature<%>)
    (field [max-hp 1])
    (field [hp 1])
    (field [defense 5])
    (field [attack-skill 0])
    (super-new)

    (define/public (get-current-defense) defense)
    (define/public (get-name) "Bloodleech")
    (define/public (get-name-with-article) "a Bloodleech")
    (define/public (get-description) "It's a muscular worm about the size of your forearm. Its moist skin glistens.")
    (define/public (get-brawl-damage) 1)
    (define/public (get-next-action)
      (make-action #:symbol 'attack
                   #:actor this
                   #:duration 1
                   #:target 'pc
                   #:tags '(delayed-resolution)))
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'dead)
                 'hit)))
    (define/public (get-status) (error "creatures.rkt: bloodleech%: get-status not implemented yet!"))))

(define blindscraper%
  (class* object% (creature<%>)
    (field [max-hp 2])
    (field [hp 2])
    (field [defense 7])
    (field [attack-skill 1])
    (super-new)
    
    (define/public (get-current-defense) defense)
    (define/public (get-name) "Blindscraper")
    (define/public (get-name-with-article) "a Blindscraper")
    (define/public (get-description) "It's vaguely insect-like, a tangly mess of appendages. At the end of each of its seven fingers there's a claw.")
    (define/public (get-brawl-damage) (d 1 3))
    (define/public (get-next-action)
      (define roll (d 1 2))
      (cond ((= roll 1)
             (make-action #:symbol 'wait
                          #:actor this
                          #:duration 1
                          #:target null
                          #:tags '(delayed-resolution))
             )
            ((= roll 2)
             (make-action #:symbol 'attack
                          #:actor this
                          #:duration 1
                          #:target 'pc
                          #:tags '(delayed-resolution)))))
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'dead)
                 'hit)))
    (define/public (get-status) (error "creatures.rkt: blindscraper%: get-status not implemented yet!"))))

(define grabberkin%
  (class* object% (creature<%>)
    (field [max-hp 6])
    (field [hp 6])
    (field [defense 4])
    (field [attack-skill -1])
    (super-new)
    
    (define/public (get-current-defense) defense)
    (define/public (get-name) "Grabberkin")
    (define/public (get-name-with-article) "a Grabberkin")
    (define/public (get-description) "TODO")
    (define/public (get-brawl-damage) (- (d 1 2) 1))
    (define/public (get-next-action)
      (define roll (d 1 1))
      (cond ((= roll 1)
             (make-action #:symbol 'attack
                          #:actor this
                          #:duration 1
                          #:target 'pc
                          #:tags '(delayed-resolution)))))
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'dead)
                 'hit)))
    (define/public (get-status) (error "creatures.rkt: blindscraper%: get-status not implemented yet!"))))

(provide (all-defined-out))