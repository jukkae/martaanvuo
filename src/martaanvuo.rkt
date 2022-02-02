#lang at-exp racket

(require "fragments/fragments.rkt") ; requiring fragments has side effects and is necessary

(require "resolvers/game-resolver.rkt")


(define (begin-session)
  ; (random-seed 13)

  (if (file-exists? "save.txt")
      (resolve-game 'continue)
      (resolve-game 'begin)))

(begin-session)
