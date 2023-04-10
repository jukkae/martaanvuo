#lang racket

(provide martaanvuo-version)
(define martaanvuo-version "0.0.8")

(provide project-root-path)
(define project-root-path (this-file))

(define-syntax (this-file stx)
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #'(path-only file)])))
