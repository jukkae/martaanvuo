#lang at-exp racket

(provide (all-defined-out))

(require "../../core/api.rkt")


(fragment
 'waiting-room-begin
 (thunk
  (p @~a{
 The waiting room is a short corridor, its walls painted in stark white, coldly lit by a flickering fluorescent. There is a single wood-framed chair along one wall. A bit further down the corridor, there's a single nondescript door.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-1
        )))

(fragment
 'waiting-room-1
 (thunk
  (p @~a{
 There's the soft whirr and occasional clanks of air systems. Might as well sit down.

 Otava sits down. The chair creaks as its frame slightly flexes. It is not an uncomfortable chair. Its thinly padded seat is upholstered in sturdy, bluish gray fabric.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-2
        )))

(fragment
 'waiting-room-2
 (thunk
  (p @~a{
 The chair creaks again, as Otava shifts her position. Nothing to do but wait.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-3
        )))

(fragment
 'waiting-room-3
 (thunk
  (p @~a{
 She waits a bit longer.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-4
        )))

(fragment
 'waiting-room-4
 (thunk
  (p @~a{
 How long has it been now? Fifteen minutes? Half an hour? Something like that. She drums her fingers together, tries a couple of new configurations how to put them next to each other.

 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-5
        )))

(fragment
 'waiting-room-5
 (thunk
  (p @~a{
 A while passes.

 Then, she waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-end
        )))

(fragment
 'waiting-room-end
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-end
        )))