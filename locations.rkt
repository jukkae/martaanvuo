#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

; locations

(define location<%> (interface () get-description get-interactions get-visible-exits))
(define forest%
  (class* object% (location<%>)
    (define times-described 0)
    (define searched? #f)
    (super-new)

    (define/public (get-nth-description n)
      (cond ((= n 1) "You are walking through a dense coniferous forest. It is bitterly cold.")
            ((= n 2) "The Sun has come up a while ago. Her pale light scarcely filters through the branches of age-old pines.")
            ((= n 3) "You notice you are walking along a path, too narrow to be human, yet sharp, intelligent.")
            ((= n 4) "You are walking along a forest path.")
            ((= n 5) "You are walking along a forest path. You hear a RIVER to your right.")
            ((= n 6) "You are walking along a forest path. You hear a river to your right.")
            (else "You are utterly lost in the Dead Woods. Or was it the Woods of Dead? The woods seem very much alive to you.")))
      
    (define/public (get-description)
      (begin (set! times-described (add1 times-described))
             (get-nth-description times-described)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))

    (define/public (get-visible-exits)
      #(cond ((< times-described 5) (list (make-action 'go-on "Keep on walking." 1 null '(wilderness))))
             ((> times-described 4 (list (make-action "Keep on walking." 'go-on 1 null '(wilderness))
                                         (make-action "Go right." 'go-right 1 null '(wilderness)))))
             (else '()))
      (if (< times-described 5)
          (list (make-action 'go-on "Keep on walking." 1 null '(wilderness)))
          (list (make-action 'go-on "Keep on walking." 1 null '(wilderness)))))

    (define/public (search)
      
      (define roll (d 2 6))
      (newline)
      (displayln (string-append "The area looks promising, so you take a look around." "[2d6: " (number->string roll) "]" ))
      
      (set! searched? #t)
      (define target-number 4)
      (define critical 10)
      (define loot (cond ((> roll critical) (new amulet%))
                         ((> roll target-number) (new knife%))
                         (else 'nothing)))
      loot)
    (define/public (camp)
      (displayln "---"))))

(provide (all-defined-out))