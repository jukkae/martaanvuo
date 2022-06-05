#lang racket

(require racket/gui/base)

(define window
  (new frame%
    [label "Martaanvuo editor"]
    [width 900]
    [height 600]))

(define container
  (new horizontal-panel%
    (parent window)
    (style (list 'border))))



(define fragment-file-selector
  (new list-box%
    (label "Fragments")
    (parent (new vertical-panel%
                  (parent container)
                  (style (list 'border))))
    (choices (list "Item 0"
                    "Item 1"
                    "Item 2"))
    (style (list 'single
                  'column-headers))
    (columns (list "First Column"))))



(define content-browser
  (new panel%
    [parent container]))

(define (change-tab tp event)
  (when (eq? (send event get-event-type) 'tab-panel)
    (fill-tab-content tp)))

(define (fill-tab-content tp)
  (define current-tab-name
      (send tp get-item-label (send tp get-selection)))
  (case current-tab-name
   [("Fragments") (displayln "FRAGMENTS")]
   [("Narration") (displayln "NARRATION")]
   [("World") (displayln "WORLD")]
   ['else (displayln "UNKNOWN")])
  (send tp change-children
        (lambda (c*)
          (list
            (new message%
                [label (~a "You have selected: " current-tab-name)]
                [parent tp])))))

(define tab-panel
  (new tab-panel%
    [parent content-browser]
    [choices (list "Fragments"
                  "Narration"
                  "World")]
    [callback change-tab]))

(send content-browser show #t)
(fill-tab-content tab-panel)







(define editor-canvas
  (new editor-canvas%
      (parent container)
      (label "Editor Canvas")))

(define text (new text%))
(send text insert "Editor Canvas")
(send editor-canvas set-editor text)

(send window show #t)
