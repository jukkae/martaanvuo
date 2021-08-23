#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 timeline
 (metadata
  events
  duration))