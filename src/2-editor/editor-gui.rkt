#lang racket/base

;;==========================================================================
;;===                Code generated with MrEd Designer 3.17              ===
;;===              https://github.com/Metaxal/MrEd-Designer              ===
;;==========================================================================

;;; Call (editor-gui-init) with optional arguments to this module

(require
 racket/gui/base
 racket/class
 racket/list
 )

(provide editor-gui-init
         main-window
         fragment-file-selector
         fragment-editor-canvas)

(define (label-bitmap-proc l)
  (let ((label (first l)) (image? (second l)) (file (third l)))
    (or (and image?
             (or (and file
                      (let ((bmp (make-object bitmap% file 'unknown/mask)))
                        (and (send bmp ok?) bmp)))
                 "<Bad Image>"))
        label)))

(define (list->font l)
  (with-handlers
   ((exn:fail?
     (λ (e)
       (send/apply
        the-font-list
        find-or-create-font
        (cons (first l) (rest (rest l)))))))
   (send/apply the-font-list find-or-create-font l)))

(define editor-gui #f)
(define main-window #f)
(define tab-panel-20641 #f)
(define tab-fragments #f)
(define tab-fragments-layout #f)
(define fragment-file-selector #f)
(define fragment-selector #f)
(define fragment-editor-canvas #f)
(define tab-descriptions #f)
(define tab-world #f)
(define (editor-gui-init
         #:main-window-width
         (main-window-width 1200)
         #:main-window-height
         (main-window-height 800)
         #:fragment-file-selector-callback
         (fragment-file-selector-callback
          (lambda (list-box control-event) (void)))
         #:fragment-selector-callback
         (fragment-selector-callback (lambda (list-box control-event) (void))))
  (set! main-window
    (new
     frame%
     (parent editor-gui)
     (label "Martaanvuo editor")
     (width main-window-width)
     (height main-window-height)
     (x #f)
     (y #f)
     (style '())
     (enabled #t)
     (border 0)
     (spacing 0)
     (alignment (list 'center 'top))
     (min-width 70)
     (min-height 30)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! tab-panel-20641
    (new
     (class tab-panel%
       (super-new)
       (define child-panels '())
       (define/public
        (add-child-panel p label)
        (set! child-panels (append child-panels (list p)))
        (send this append label)
        (when (> (length child-panels) 1) (send this delete-child p)))
       (define/public
        (active-child n)
        (send this change-children
          (lambda (children) (list (list-ref child-panels n))))))
     (parent main-window)
     (choices (list))
     (callback (λ (tp e) (send tp active-child (send tp get-selection))))
     (style '())
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'left 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! tab-fragments
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent parent))
       (send parent add-child-panel this label))
     (parent tab-panel-20641)
     (label "Fragments")
     (style '())
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'left 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! tab-fragments-layout
    (new
     horizontal-pane%
     (parent tab-fragments)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'left 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! fragment-file-selector
    (new
     list-box%
     (parent tab-fragments-layout)
     (label "Fragment files")
     (choices (list "foo" "bar"))
     (callback fragment-file-selector-callback)
     (style
      ((lambda (l) (list* (first l) (second l) (third l)))
       (list 'single 'vertical-label '())))
     (font
      (list->font
       (list 12 "Helvetica" 'default 'normal 'normal #f 'default #f)))
     (selection 0)
     (enabled #t)
     (vert-margin 2)
     (horiz-margin 2)
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)
     (columns (list "Column"))
     (column-order #f)))
  (set! fragment-selector
    (new
     list-box%
     (parent tab-fragments-layout)
     (label "Fragments")
     (choices (list "First" "Second"))
     (callback fragment-selector-callback)
     (style
      ((lambda (l) (list* (first l) (second l) (third l)))
       (list 'single 'vertical-label '())))
     (font
      (list->font
       (list 12 "Helvetica" 'default 'normal 'normal #f 'default #f)))
     (selection 0)
     (enabled #t)
     (vert-margin 2)
     (horiz-margin 2)
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)
     (columns (list "Column"))
     (column-order #f)))
  (set! fragment-editor-canvas
    (new
     editor-canvas%
     (parent tab-fragments-layout)
     (editor #f)
     (style '())
     (scrolls-per-page 100)
     (label "Editor-Canvas")
     (wheel-step 3)
     (line-count #f)
     (horizontal-inset 5)
     (vertical-inset 5)
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! tab-descriptions
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent parent))
       (send parent add-child-panel this label))
     (parent tab-panel-20641)
     (label "Descriptions")
     (style '())
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'left 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! tab-world
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent parent))
       (send parent add-child-panel this label))
     (parent tab-panel-20641)
     (label "World")
     (style '())
     (enabled #t)
     (vert-margin 0)
     (horiz-margin 0)
     (border 0)
     (spacing 0)
     (alignment (list 'left 'center))
     (min-width 0)
     (min-height 0)
     (stretchable-width #t)
     (stretchable-height #t)))
  (send main-window show #t))

(module+ main (editor-gui-init))
