#lang racket

(provide (all-defined-out))

(require
  "../actions/action.rkt"

  "../actors/actor.rkt"

  "../core/utils.rkt"

  "../state/state.rkt")

(define (make-blindscraper)
  (define enemy (make-actor "Blindscraper" 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
  enemy)

(define (make-blindscraper-action actor action-flag)
  (case action-flag

    ['attack
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'melee
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]

    ['go-to-engaged
     (make-action
      #:symbol 'go-to-engaged
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['go-to-close
     (make-action
      #:symbol 'go-to-close
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['blindscrape
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '(blind))]

    [else
     (error (format "make-blindscraper-action: unknown action: ~a" action-flag))]))

(define (get-blindscraper-action actor)
  (cond ((in-combat?)
         (cond
           ((> (actor-hp actor) 1)

            (cond
              ((actor-in-range? actor 'engaged)
               (define options
                 (list
                  (cons 1 'blindscrape)
                  (cons 1 'blindscrape)
                  (cons 1 'blindscrape)
                  (cons 1 'blindscrape)
                  #;(cons 2 'attack)
                  #;(cons 3 'attack)
                  #;(cons 4 'attack)))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag (cdr action-flag-with-index))
               (make-blindscraper-action actor action-flag))

              ((actor-in-range? actor 'close)
               (define options
                 (list
                  (cons 1 'attack)
                  (cons 2 'attack)
                  (cons 3 'go-to-engaged)
                  (cons 4 'go-to-engaged)
                  #;(cons 4 'parry)
                  ))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag (cdr action-flag-with-index))
               (make-blindscraper-action actor action-flag))

              (else
               (define options
                 (list
                  (cons 1 'attack)
                  (cons 2 'attack)
                  (cons 3 'go-to-engaged)
                  (cons 4 'go-to-engaged)
                  #;(cons 4 'parry)
                  ))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag 'go-to-close)
               (make-blindscraper-action actor action-flag))))

           ((= (actor-hp actor) 1)
            (make-action
             #:symbol 'flee
             #:actor actor
             #:duration 1
             #:target '()
             #:tags '(initiative-based-resolution fast)
             #:details '()))))
        (else
         (begin (displayln "Blindscraper AI, not in combat")))))

(define (get-blindscraper-reaction actor)
  '())
