#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action-resolver.rkt")
(require "action.rkt")
(require "actor.rkt")
(require "io.rkt")
(require "situation.rkt")
(require "stance.rkt")
(require "status.rkt")
(require "utils.rkt")
(require "world.rkt")


(define (make-grabberkin-action actor action-flag)
  (case action-flag
    ['pull-under
     (make-action
      #:symbol 'pull-under
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['anklebreaker
     (make-action
      #:symbol 'anklebreaker
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]
    
    ['skip
     (make-action
      #:symbol 'skip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(slow silent))]

    ['grab
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 0
      #:target (pc)
      #:tags '(initiative-based-resolution fast)
      #:details (list (status 'bound 10)))]

    ['release-grip
     (make-action
      #:symbol 'release-grip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(fast))]

    [else
     (error (string-append
             "make-grabberkin-action: unknown action: "
             (symbol->string action-flag)))]))

(define (grabberkin-hp-above-threshold? actor)
  (define hp (actor-hp actor))
  (define max-hp (actor-max-hp actor))
  (define threshold (quotient max-hp 2))
  
  (> hp threshold))

(define (get-grabberkin-action actor)
  (cond ((in-combat?)
         (cond
           ((grabberkin-hp-above-threshold? actor)
            (cond
              ((and (actor-in-range? actor 'engaged)
                    (actor-has-status-of-type? (pc) 'bound)
                    (> (actor-lifetime-of-status-of-type? (pc) 'bound)
                       4))
               (define options
                 (list
                  (cons 1 'pull-under)
                  (cons 2 'anklebreaker)
                  (cons 3 'anklebreaker)
                  (cons 4 'skip)
                  (cons 5 'skip)
                  (cons 6 'skip)))
               
               (define roll (d 1 6))
               (define index (- roll 1))
               (define action-flag-with-index (list-ref options index))

               
               (define action-flag (cdr action-flag-with-index))
               
               (make-grabberkin-action actor action-flag))
              (else
               (make-grabberkin-action actor 'grab))))
           
           (else
            (make-grabberkin-action actor 'release-grip))))
        (else
         (begin (displayln "Grabberkin AI, not in combat")))))

; implicitly, this is the pre-own-action reaction
(define (get-grabberkin-reaction actor)
  (cond ((not (grabberkin-hp-above-threshold? actor))
         (make-grabberkin-action actor 'release-grip))
        (else
         '())))

;;; TO DO:
;;; THE BIG THINGS:
;;; What is Grabberkin's purpose, gameplay-wise? What is its niche?
;;; What is its vulnerability?
;;; How much leeway there is? Should it be considered a mini-boss?
;;; Idea: Easy to win once you know how, but requires something specific that can be gotten easily if you know how
;;; -> act as a soft gate
;;; -> somewhat miniboss-like *at first*
; Idea: Vulnerabilities:
; - Firearms: Simply too much firepower. (Approach weaknesses: Makes noise, bullets are hard to come by, fuck up and blow your own leg up -> skill check if not proficient / consume LP)
; - Chainsaw: Too awesome. Weaknesses: Got to get up-and-close, makes noise, needs gas
; - Slashing damage, IF you know how to do it - because it cuts through tendons -> can't grab no more
; – Something purely mechanical, stun?
; - Good perception (-> avoid)

; -> when selecting loadout, possibilities are bolt cutters or a chainsaw,
; gun has to be sourced by doing a favor to The Merchant, robbing a Cache, or finding your own corpse that had a gun in a prior run.
; guns: hunting rifles and shotguns in the wild, at the Facility also handguns and assault rifles.

; This doesn't belong here really but what the hell:
; - After combat, patching up is possible, but it is risk-free only if you know medicine, which requires finding literature (= doing a Cache-run, in practice) and then leveling up

(define (spawn-grabberkin-encounter!)
  ; TODO usually grab only one ankle, sometimes both
  (paragraph "Something grabs Otava by the ankle and pulls. She staggers, barely manages to stay upright, and immediately goes for her bolt cutters.") ; could cause fall-down on failed roll
  (set-in-combat?! #t)

  (define hp 11)
  (define i 0)
  (define enemy (make-actor "Grabberkin" hp))
  (set-actor-dexterity! enemy 4)
  (set-actor-strength! enemy 11)
  (set-trait! enemy "defense" -1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "hp-hidden" #t)
  (move-actor-to-location! enemy (current-location))

  (inflict-status! (pc) (status 'bound 10))

  (define index
    (case i
      [(0) "α"]
      [(1) "β"]))
  (define range 'engaged)
  (define location "grabbing Otava's ankle")
  (define enemy-stance
    (stance index range location))
           
  (hash-set! (situation-enemy-stances *situation*) enemy enemy-stance)
  )