#lang at-exp racket

(require
  racket/lazy-require
  )

(provide
  append-to-log

  current-chapter
  current-combat-timeline
  current-in-combat?
  current-last-numeric-actor-id
  current-last-numeric-actor-id++
  current-last-paragraph
  current-location
  current-log
  current-part
  current-pending-action
  current-prompt
  current-tasks
  current-world


  display-combatant-info
  get-current-enemies
  get-next-numeric-actor-id
  pc
  )


(lazy-require ["../7-state/state/logging.rkt" (
  append-to-log
  )])

(lazy-require ["../7-state/state/state.rkt" (
  current-chapter
  current-combat-timeline
  current-in-combat?
  current-last-numeric-actor-id
  current-last-numeric-actor-id++
  current-last-paragraph
  current-location
  current-log
  current-part
  current-pending-action
  current-prompt
  current-tasks
  current-world


  display-combatant-info
  get-current-enemies
  get-next-numeric-actor-id
  pc
  )])
