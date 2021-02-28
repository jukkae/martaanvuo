#lang racket/base

(require roman-numeral)
(require "utils.rkt")

(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "==================="))

(define (print-inventory inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (displayln (string-append "In addition to clothes, you have " inventory ".")))

(define (narrate-run-number number)
  (newline)
  (newline)
  (displayln (string-append "RUN " (string-upcase (number->roman number))))
  (newline)
  (displayln "A sense of self emerges from the Dark. You arise in")
  (displayln "M A R T A A N V U O.")
  (newline))

(define (narrate-startup)
  (newline)
  (displayln "You should be able to select a previous save. You can not.")
  (newline)
  (displayln "A new game begins."))

(define (narrate-quit)
  (displayln "You quit. For now.")
  (newline)
  (displayln "Your progress should be saved. It is not."))

(define (get-curse)
  (define index 0 #;(random 2))
  (cond ((= index 0) (define first '("Rot" "Blight" "Pus" "Pain" "Snow" "Rain" "Frost"))
                     (define second '("decay" "corrosion" "death" "destruction" "sorrow" "suffering"))
                     (string-append (take-random first) " and " (take-random second) "!"))
        (else (take-random '("Let it all wither!"
                             "May it all languish!"
                             "Blight!"
                             "Scales of a snake!")))))

(define (get-narration-for-stab)
  (take-random '("You go for a stab. Aim at the soft underbelly."
                 "You lean in to stab. Put your weight behind it, pierce the scourge."
                 "You slash."
                 "You stab."
                 "You go for a stab."
                 "You lean in to stab.")))

(define (get-narration-for-successful-stab)
  (take-random '("Your stab connects."
                 "Your stab lands with a satisfying thud."
                 "Your blade pierces the skin of the enemy.")))

(define (get-narration-for-brawl)
  (take-random '("You grapple with the enemy. Try to get it pinned."
                 "You wrestle."
                 "You try to get it pinned. That is the Way of the Anthead of Riverfrost."
                 #;"You try to strangle."
                 #;"Get it pinned, then skinned."
                 #;"Get it pinned, then skinned. Came up with that myself."
                 #;"Not the first neck I've broken.")))

(define (get-narration-for-successful-brawl)
  (take-random '("Snap. You feel a crack under your fingers."
                 "Crunch."
                 "Crack."
                 "Cronk. You feel something thick break under your hands."
                 "Puff. The snow billows up as you throw down your enemy under you.")))

(provide (all-defined-out))