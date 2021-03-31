#lang racket

(require racket/struct)
(require lens)

(define-struct choice
  (symbol
   name
   resolution-effect))

(struct/lens action
 
             (symbol
              actor
              duration
              target
              tags)
             #:constructor-name action*

             #:methods gen:custom-write
             [(define write-proc
                (make-constructor-style-printer
                 (lambda (obj) 'action)
                 (lambda (obj)
                   (list
                    (unquoted-printing-string "symbol: ")
                    (action-symbol obj)
                    (unquoted-printing-string "actor: ")
                    (action-actor obj)
                    (unquoted-printing-string "duration: ")
                    (action-duration obj)
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
  (action* symbol actor duration target tags))

(define (visible-in-combat? action)
  (if (or (member 'combat (action-tags action))
          (member 'always (action-tags action)))
      #t
      #f))

(define (free? action)
  (if (member 'free (action-tags action))
      #t
      #f))

(define (resolve-instantly? action)
  (if (not (member 'delayed-resolution (action-tags action)))
      #t
      #f))

(define (has-tag? action tag)
  (memq tag (action-tags action)))

; return true if first is less, ie., sorted earlier, than second
; ie., #t = action1 is faster than action2
(define (action-faster-than? action1 action2)
  (cond ((has-tag? action1 'slow) #f)
        ((has-tag? action1 'slow) #t)
        ((eq? (action-actor action1) 'pc) #t)
        ((eq? (action-actor action2) 'pc) #f)))


(provide (all-defined-out))