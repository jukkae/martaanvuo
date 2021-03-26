#lang racket

(require racket/struct)

(define-struct choice
  (symbol
   name
   resolution-effect))

(define-struct action
  (symbol
   actor
   duration
   elapsed
   target
   tags)
  #:constructor-name action*

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'action)
      (lambda (obj)
        (list (unquoted-printing-string "symbol: ")
              (action-symbol obj)
              (unquoted-printing-string "actor: ")
              (action-actor obj)
              (unquoted-printing-string "duration: ")
              (action-duration obj)
              (unquoted-printing-string "elapsed: ")
              (action-elapsed obj)
              (unquoted-printing-string "target: ")
              (action-target obj)
              (unquoted-printing-string "tags: ")
              (action-tags obj)
              ))))])

(define (make-action
         #:symbol symbol
         #:actor actor
         #:duration duration
         #:target target
         #:tags tags)
  (action* symbol actor duration 0 target tags))

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