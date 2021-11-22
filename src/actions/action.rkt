#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "../core/utils.rkt")

(lazy-require ["../state/state.rkt"
               (pc)])

(serializable-struct action
 
                     (symbol
                      actor
                      [duration #:mutable]
                      target
                      tags
                      [details #:mutable])
                     #:constructor-name action*
                     #:transparent
                     )

(define (make-action
         #:symbol symbol
         #:actor actor
         #:duration duration
         #:target [target '()]
         #:tags [tags '()]
         #:details [details '()])
  (action* symbol actor duration target tags details))

(define (make-empty-action)
  (make-action
   #:symbol 'skip
   #:actor (pc)
   #:duration 0
   #:tags '(downtime)))

(define (visible-in-combat? action)
  (if (or (member 'combat (action-tags action))
          (member 'always (action-tags action)))
      #t
      #f))

(define (pending? action)
  (if (or (eq? 'pending (action-details action))
          (if (list? (action-details action))
              (member 'pending (action-details action))
              #f
              ))
      #t
      #f))

(define (free? action)
  (if (member 'free (action-tags action))
      #t
      #f))

(define (immediate-resolution? action)
  (if (not (member 'delayed-resolution (action-tags action)))
      #t
      #f))

(define (initiative-based-resolution? action)
  (if (member 'initiative-based-resolution (action-tags action))
      #t
      #f))

(define (aggressive? action)
  (cond ((eq? (action-symbol action) 'shoot) #t)
        ((eq? (action-symbol action) 'melee) #t)
        ((has-tag? action 'aggressive) #t)
        (else #f)))

(define (has-tag? action tag)
  (memq tag (action-tags action)))

; return true if first is less, ie., sorted earlier, than second
; ie., #t = action1 is faster than action2
(define (action-faster-than? action1 action2)
  (cond ((has-tag? action1 'slow) #f)
        ((has-tag? action2 'slow) #t)
        ((has-tag? action1 'fast) #t)
        ((has-tag? action2 'fast) #f)
        ((eq? (action-actor action1) 'pc) #t)
        ((eq? (action-actor action2) 'pc) #f)))

(define (all-actions-of-type? actions type)
  (define predicate
    (λ (action) (eq? (action-symbol action) type)))
  (all-fulfill-predicate? actions predicate))