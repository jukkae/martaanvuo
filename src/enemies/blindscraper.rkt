#lang at-exp racket

(provide (all-defined-out))

(require
  "../actions/action.rkt"

  "../actors/actor.rkt"
  "../actors/pc-actor.rkt"

  "../combat/stance.rkt"

  "../core/checks.rkt"
  "../core/io.rkt"
  "../core/utils.rkt"

  "../state/state.rkt")

(define (make-blindscraper)
  (define enemy (make-actor "Blindscraper" 'blindscraper 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
  enemy)

(define (make-blindscraper-action actor action-flag)
  (case action-flag

    ['attack
     (define damage-roll '(λ () (d 1 2)))
     (define details
       `(list
         (cons 'damage-roll ,damage-roll)
         (cons 'damage-roll-formula "1d2")
         ))
     (make-action
      #:symbol 'melee
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details
      #:resolution-rules
      `(
        (displayln (format "ACTOR: ~a" (actor-name ,actor)))
        (resolve-melee-action! ,actor (pc) ,details)
        ))]

    ['go-to-engaged
     (make-action
      #:symbol 'go-to-engaged
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '()
      #:resolution-rules
      `(
        (define lp (pc-actor-lp (pc)))
        (define dex (,actor-dexterity ,actor))
        (define success?
          (cond ((positive? lp)
                 (displayln "[LP positive]")
                 (attribute-check "Dexterity" dex))
                (else #t)))

        (if success?
            (begin
              (p "The Blindscraper suddenly leaps forward and gets a hold of Otava's forearm with a couple of its lanky fingers. One of its long claws is swinging free, looking for an opening.")

              (let ([enemy-stance (stance "α" 'engaged "right")])
                (set-actor-stance! ,actor enemy-stance)))

            (begin
              (p "The Blindscraper leaps at Otava, but she dives under it and stumbles back to her feet.")
              (displayln "[-1 LP]")
              (set-pc-actor-lp! (pc)
                                (- (pc-actor-lp (pc))
                                   1))
              (when (< (pc-actor-lp (pc)) 0)
                (set-pc-actor-lp! (pc)
                                  0))
              (displayln (pc-actor-lp (pc)))
              'failure))
        'ok
        )
      )]

    ['go-to-close
     (make-action
      #:symbol 'go-to-close
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '()
      #:resolution-rules
      (λ ()
        (define lp (pc-actor-lp (pc)))
        (define dex (actor-dexterity actor))

        (p "The Blindscraper skitters towards Otava.")

        (let ([enemy-stance (stance "α" 'close "right")])
          (set-actor-stance! actor enemy-stance))
        'ok
        ))]

    ['blindscrape
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '(blind)
      #:resolution-rules
      (λ ()
        (define target (pc))
        (inflict-status! (pc) 'blind))
      )]

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
                  (cons 2 'attack)
                  (cons 3 'attack)
                  (cons 4 'attack)))
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
             #:details '()
             #:resolution-rules
             `((resolve-flee-action! ,actor))))))
        (else
         (begin (displayln "Blindscraper AI, not in combat")))))

(define (get-blindscraper-reaction actor)
  '())
