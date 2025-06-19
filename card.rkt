#lang racket

(provide all-cards dummy-card%)

(require "ui.rkt")

(define card
  (interface ()
             resolve
             discard))

;TODO: Put dummy-card% in a test module
(define dummy-card%
  (class* object% (card)
    (super-new)

    (init-field name)

    (define/public (resolve the-board)
      ((ui) `(resolving-card ,name))
      the-board)

    (define/public (discard the-board)
      ((ui) `(discarding graveyard ,name))
      (send the-board discard this #:stack 'graveyard))))
      

(define all-cards
  (list (new dummy-card% [name 'a])
        (new dummy-card% [name 'b])
        (new dummy-card% [name 'c])))
