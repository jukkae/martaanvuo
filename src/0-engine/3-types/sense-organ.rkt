#lang typed/racket

(provide (struct-out SenseOrgan))

(struct SenseOrgan ([id : Symbol] [level : Integer] [name : String]) #:prefab #:mutable)
