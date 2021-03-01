#lang racket

(define-struct action (symbol
                       name
                       duration
                       target
                       tags))

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


(provide (all-defined-out))