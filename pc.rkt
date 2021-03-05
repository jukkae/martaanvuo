#lang racket

(require "actions.rkt")
(require "actors.rkt")
(require "items.rkt")
(require "utils.rkt")
; see also lazy-require and units

(define pc%
  (class* object% (actor<%>)
    (field [hp 4])
    (field [attack-skill 1])
    (field [inventory (list (new twine%))])

    (super-new)

    (define/public (get-brawl-damage) (d 1 2))
    (define/public (get-next-command world) '())
    (define/public (get-generic-actions world)
      (if (not (empty? (get-field inventory this)))
          (list (make-action 'inventory "Show inventory. [free action]" 0 null '(always free))) ; tag - duration in jiffies - object - list of tags
          '()))
    
    (define/public (get-combat-actions world)
      (define targets (send world get-current-enemies))
      
      (define combat-actions '())
      (if (get-field in-combat world)
          (begin
            (for ([i (in-range 0 (length targets))])
              (displayln i)
              (define target (list-ref targets i))
              (displayln target)
              (set! combat-actions
                    (append combat-actions
                            (list (make-action 'brawl
                                               (string-append "Brawl with " (send target get-name) " (number " (number->string i) ")")
                                               1
                                               target
                                               '(combat))))))
            
            (set! combat-actions (append combat-actions (list (make-action 'run "Run." 1 null '(combat))))))
          
          '())
      combat-actions)
    
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'u-ded)
                 'u-hit)))
    (define/public (get-status) (error "not implemented yet!"))
    (define/public (get-a-hunch)
      (take-random '(
                     "You feel something is watching you.")))))


(define *actors* (list (new pc%)))


(provide (all-defined-out))