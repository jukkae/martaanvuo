#lang at-exp racket

(require reprovide/reprovide)
(require reprovide/require-transformer/glob-in)

(reprovide (glob-in "../3-types/*.rkt"))
