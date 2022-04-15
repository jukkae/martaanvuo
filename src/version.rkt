#lang racket

(provide martaanvuo-version)
(define martaanvuo-version "0.0.2-preview")

; TODO: Either this doesn't belong in this file, or this file has the wrong name
(provide project-root-path)
(define project-root-path (this-file))

(define-syntax (this-file stx)
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #'(path-only file)])))