#lang racket

(define-struct choice
  (symbol
   name
   resolution-effect))

(define-struct action
  (symbol
   actor
   duration
   target
   tags)
  #:constructor-name action*)

(define (make-action
         #:symbol symbol
         #:actor actor
         #:duration duration
         #:target target
         #:tags tags)
  (action* symbol actor duration target tags))

(define (is-visible-in-combat? action)
  (if (or (member 'combat (action-tags action))
          (member 'always (action-tags action)))
      #t
      #f))

(define (is-free? action)
  (if (member 'free (action-tags action))
      #t
      #f))

(define (resolve-instantly? action)
  (if (not (member 'delayed-resolution (action-tags action)))
      #t
      #f))

(define (has-tag? action tag)
  (memq tag (action-tags action)))


(provide (all-defined-out))