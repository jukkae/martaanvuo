#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(require
  "../../../0-engine/3-types/choice.rkt")

(add-place!
  (place
    #:id 'forge
    #:shortname "Forge of Master Seppo"))

(add-route-between! 'ladder-of-surut 'forge 1)
