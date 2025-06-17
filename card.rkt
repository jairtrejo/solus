#lang racket

(provide all-cards dummy-card%)

(define card
  (interface ()
             resolve))

(define dummy-card%
  (class* object% (card)
    (super-new)

    (init-field name)

    (define/public (resolve ui the-board)
      (ui `(resolving-card ,name))
      the-board)))
      

(define all-cards
  (list (new dummy-card% [name 'a])
        (new dummy-card% [name 'b])
        (new dummy-card% [name 'c])))
