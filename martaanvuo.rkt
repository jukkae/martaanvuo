#lang racket

(require "commands.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "minds.rkt")
(require "narration.rkt")
(require "pc.rkt")
(require "utils.rkt")
(require "world.rkt")

; globals and state
(define *metaloop* 0)
(define (reset-meta)
  (reset-state)
  (set! *metaloop* (add1 *metaloop*)))

(define (quit)
  (narrate-quit)
  (exit))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (meta-command? action)
  (eq? 'meta-command (car action)))

(define (get-next-action actor)
  (when (is-a? actor player-actor%)
    (displayln "PLAYER MIND")
    (define world-actions (get-world-actions *world* actor))
    (displayln "--- get possible world actions from world and actor")
    (displayln "--- build key-to-action-map for proper actions")
    (displayln "--- calculate relevant meta actions")
    (displayln "--- get input")
    (displayln "--- handle meta-actions here (-> game state stack)")
    (displayln "--- if not meta-action, then proper action")
    (displayln "--- treat input as action request to world and actor"))
  (define command (send actor get-next-command *world*))
  (if (meta-command? command)
      (displayln "-- meta-command")
      (displayln "-- proper action")))





(define (resolve-turn)
  (begin-turn! *world*)
  (describe-situation *world*)
  ; TODO sort by initiative
  (define actions (map get-next-action *actors*))
  (resolve-actions! *world* actions)
  (end-turn! *world*)
  (resolve-turn))




(define (end-game)
  (newline)
  (display "Do you want to try again? [Q] to quit, [R] to restart."))


(define (restart) (meta-loop))

(define (meta-loop)
  ;begin new run
  (reset-meta)
  (narrate-run-number *metaloop*)

  (resolve-turn)
  (error "meta-loop: resolve-turn should not exit recursion"))

(define (startup)
  (title)
  (narrate-startup)
  (call/cc (end-game (meta-loop))))

(startup)