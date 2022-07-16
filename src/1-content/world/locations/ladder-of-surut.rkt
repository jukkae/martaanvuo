#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(add-place!
  (place
    #:id 'forge
    #:shortname "Forge of Master Seppo"))

(add-route-between! 'ladder-of-surut 'forge 1)
