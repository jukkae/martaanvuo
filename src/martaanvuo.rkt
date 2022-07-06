#lang at-exp racket

(require "0-engine/5-resolvers/0-game-resolver/game-resolver.rkt")
(require "1-content/fragments/fragments.rkt")

(define (begin-session)
  (if (file-exists? "save.txt")
      (resolve-game 'continue)
      (resolve-game 'begin #:scenario 'ladder-of-surut)))

(begin-session)
