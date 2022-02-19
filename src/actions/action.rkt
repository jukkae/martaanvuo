#lang at-exp typed/racket

(provide (all-defined-out))

(require "../core/maybe.rkt")
(require "../actors/0-types/actor.rkt")
(require "../items/0-types/item.rkt")
(require "../locations/0-types/location-ids.rkt")

(require/typed "../core/utils.rkt"
               [all-fulfill-predicate? (-> (Listof Any) Procedure Boolean)])

(struct action
  ([symbol : Symbol]
   [actor-id : ActorId]
   [duration : Natural]
   [target : (Maybe (U item ActorId LocationId))]
   [tags : (Listof Symbol)]
   [details : (Listof Symbol)]
   [resolution-rules : (Maybe Sexp)]
   [on-before-rules : (Maybe Sexp)]
   [on-after-rules : (Maybe Sexp)])
  #:constructor-name action*
  #:prefab
  #:mutable
  )

(: make-action (->* (#:symbol Symbol
                     #:actor-id ActorId
                     #:duration Natural)
                    (#:target (Maybe (U item ActorId LocationId))
                     #:tags (Listof Symbol)
                     #:details (Listof Symbol)
                     #:resolution-rules (Maybe Sexp)
                     #:on-before-rules (Maybe Sexp)
                     #:on-after-rules (Maybe Sexp))
                    action))
(define (make-action
         #:symbol symbol
         #:actor-id actor-id
         #:duration duration
         #:target [target '()]
         #:tags [tags '()]
         #:details [details '()]
         #:resolution-rules [resolution-rules '()]
         #:on-before-rules [on-before-rules '()]
         #:on-after-rules [on-after-rules '()])
  (action* symbol actor-id duration target tags details resolution-rules on-before-rules on-after-rules))


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

(define (all-actions-of-type? [actions : (Listof action)] [type : Symbol])
  (define predicate
    (λ ([action : action]) (eq? (action-symbol action) type)))
  (all-fulfill-predicate? actions predicate))
