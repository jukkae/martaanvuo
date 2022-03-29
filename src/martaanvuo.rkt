#lang at-exp racket


(require "0-engine/5-resolvers/game-resolver.rkt")
(require "1-content/fragments.rkt") ; requiring fragments has side effects and is necessary

(define (begin-session)
  ; (random-seed 13)

  (if (file-exists? "save.txt")
      (resolve-game 'continue)
      (resolve-game 'begin)))

(begin-session)
