#lang racket

(require "editor-gui.rkt")

(require racket/gui/base)

(define fragment-files (directory-list "1-content/fragments"))

(define fragment-text (new text%))

(define (get-selected-fragment-file list-box)
  (define selections (send list-box get-selections))
  (if (not (null? selections)) (list-ref fragment-files (first selections)) '()))

(define (fragment-file-selected list-box event)
  (define selected-file (get-selected-fragment-file list-box))
  (define file-name (last (explode-path selected-file)))
  (define fixed-path (build-path (bytes->path #"1-content/fragments/") file-name))
  (define loaded (open-input-file fixed-path))
  (send fragment-text delete)
  (send fragment-text insert (port->string loaded)))

(editor-gui-init #:fragment-file-selector-callback fragment-file-selected)

(send fragment-file-selector clear)
(for ([file-name (map path->string fragment-files)])
  (send fragment-file-selector append file-name))

(send fragment-editor-canvas set-editor fragment-text)
