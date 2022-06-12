#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(fragment
 'teleporter-1
 @~a{
 ...
}

 #:decisions
 (list
  (make-decision
   #:title "\"...\""
   #:description "\"...\""
   #:next-fragment 'exit
   )
  ))
