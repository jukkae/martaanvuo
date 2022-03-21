#lang at-exp typed/racket

(provide (all-defined-out))

(require "../core/maybe.rkt")
(require "../actors/actor.rkt")
(require "../items/0-types/item.rkt")
(require "../locations/0-types/location-ids.rkt")

(struct action
  ([symbol : Symbol]
   [actor-id : ActorId]
   [duration : Natural]
   [target : (Maybe (U item ActorId LocationId))]
   [tags : (Listof Symbol)]
   [details : (Listof (U Symbol status Number))]
   [resolution-rules : (Maybe Sexp)]
   [on-before-rules : (Maybe Sexp)]
   [on-after-rules : (Maybe Sexp)])
  #:constructor-name action*
  #:prefab
  #:mutable
  )

(: make-action (->* (#:symbol Symbol
                     #:actor actor
                     #:duration Natural)
                    (#:target (Maybe (U item ActorId LocationId))
                     #:tags (Listof Symbol)
                     #:details (Listof (U Symbol status Number))
                     #:resolution-rules (Maybe Sexp)
                     #:on-before-rules (Maybe Sexp)
                     #:on-after-rules (Maybe Sexp))
                    action))
(define (make-action
         #:symbol symbol
         #:actor actor
         #:duration duration
         #:target [target '()]
         #:tags [tags '()]
         #:details [details '()]
         #:resolution-rules [resolution-rules '()]
         #:on-before-rules [on-before-rules '()]
         #:on-after-rules [on-after-rules '()])
  (action* symbol (actor-id actor) duration target tags details resolution-rules on-before-rules on-after-rules))

; Roll X N-sided dice, add bonus
(struct standard-damage-roll
  ([n : Integer]
   [x : Integer]
   [bonus : Integer])
  #:prefab
  #:mutable
  )

(define-type DamageType Symbol)

(: damage-roll-formula (-> standard-damage-roll String))
(define (damage-roll-formula roll)
  (define b (standard-damage-roll-bonus roll))
  (format "~ad~a~a"
          (standard-damage-roll-n roll)
          (standard-damage-roll-x roll)
          (cond [(= 0 b)
                 ""]
                [(negative? b)
                 (format "~a" b)]
                [else
                 (format "+~a" b)])))

(struct melee-attack-action
  action
  ([damage-roll : standard-damage-roll]
   [damage-type : (Maybe DamageType)])
  #:prefab
  #:mutable
  #:constructor-name melee-attack-action*
  )

(: make-melee-attack-action (->* (#:actor actor
                                  #:duration Natural
                                  #:target ActorId
                                  #:n Natural
                                  #:x Natural
                                  #:bonus Integer
                                  )
                                 (
                                  #:damage-type (Maybe DamageType)
                                  #:resolution-rules (Maybe Sexp)
                                  #:on-before-rules (Maybe Sexp)
                                  #:on-after-rules (Maybe Sexp)
                                  )
                                 action
                                 ))
(define (make-melee-attack-action
         #:actor actor
         #:duration duration
         #:target target
         #:resolution-rules [resolution-rules '()]
         #:on-before-rules [on-before-rules '()]
         #:on-after-rules [on-after-rules '()]
         #:n n
         #:x x
         #:bonus bonus
         #:damage-type [damage-type '()]
         )
  (define symbol 'melee)
  (define tags '(initiative-based-resolution))
  (define details '())
  (define attack-roll (standard-damage-roll
                       n
                       x
                       bonus))
  (melee-attack-action* symbol (actor-id actor) duration target tags details resolution-rules on-before-rules on-after-rules attack-roll damage-type))





(define (visible-in-combat? [action : action])
  (if (or (member 'combat (action-tags action))
          (member 'always (action-tags action)))
      #t
      #f))

(define (pending? [action : action])
  (if (or (eq? 'pending (action-details action))
          (if (list? (action-details action))
              (member 'pending (action-details action))
              #f
              ))
      #t
      #f))

(define (free? [action : action])
  (if (member 'free (action-tags action))
      #t
      #f))

(define (immediate-resolution? [action : action])
  (if (not (member 'delayed-resolution (action-tags action)))
      #t
      #f))

(define (initiative-based-resolution? [action : action])
  (if (member 'initiative-based-resolution (action-tags action))
      #t
      #f))

(define (aggressive? [action : action])
  (cond ((eq? (action-symbol action) 'shoot) #t)
        ((eq? (action-symbol action) 'melee) #t)
        ((has-tag? action 'aggressive) #t)
        (else #f)))

(: has-tag? (-> action Symbol Boolean))
(define (has-tag? action tag)
  (if (memq tag (action-tags action))
      #t
      #f))

; return true if first is less, ie., sorted earlier, than second
; ie., #t = action1 is faster than action2
(define (action-faster-than? [action1 : action] [action2 : action])
  (cond ((has-tag? action1 'slow) #f)
        ((has-tag? action2 'slow) #t)
        ((has-tag? action1 'fast) #t)
        ((has-tag? action2 'fast) #f)
        ((eq? (action-actor-id action1) 'pc) #t)
        ((eq? (action-actor-id action2) 'pc) #f)))


(: all-fulfill-predicate? (All (A) (-> (Listof A) (-> A Boolean) Boolean)))
(define (all-fulfill-predicate? lst predicate)
  (define result #t)
  (for ([element lst])
    (when (not (predicate element))
      (set! result #f)))
  result)

(define (all-actions-of-type? [actions : (Listof action)] [type : Symbol])
  (define predicate
    (λ ([action : action]) (eq? (action-symbol action) type)))
  (all-fulfill-predicate? actions predicate))
