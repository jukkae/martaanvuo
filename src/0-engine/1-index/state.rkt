#lang at-exp racket

(require
  racket/lazy-require
  )

(provide
  append-to-log
  next-chapter!

  current-chapter
  current-combat-timeline
  current-counters
  current-counters++
  current-fragment-id
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

  get-current-enemies
  get-next-numeric-actor-id
  pc

  unset-current-fragment-id!
  )


(lazy-require ["../7-state/logging.rkt" (
  append-to-log
  next-chapter!
  )])

(lazy-require ["../7-state/state.rkt" (
  current-chapter
  current-combat-timeline
  current-counters
  current-counters++
  current-fragment-id
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

  get-current-enemies
  get-next-numeric-actor-id
  pc

  unset-current-fragment-id!
  )])
