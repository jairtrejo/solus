#lang racket

(provide all-cards dummy-card%)

(define card
  (interface ()
             resolve
             discard))

(define dummy-card%
  (class* object% (card)
    (super-new)

    (init-field name)

    (define/public (resolve ui the-board)
      (ui `(resolving-card ,name))
      the-board)

    (define/public (discard ui the-board)
      (ui `(discarding graveyard ,name))
      (send the-board discard this #:stack 'graveyard))))
      

(define all-cards
  (list (new dummy-card% [name 'a])
        (new dummy-card% [name 'b])
        (new dummy-card% [name 'c])))
