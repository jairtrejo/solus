#lang racket


(provide board initial-board)


(require qi)
(require rackunit)
(require "stack.rkt")
(require "qi-class.rkt")
(require "card.rkt")


(define board
  (interface ()
             draw-card))


(define board%
  (class* object% (board)
    (super-new)

    (init-field [deck (list->stack '())]
                [graveyard-stack (list->stack '())]
                [lost-stack (list->stack '())])

    (define/public (draw-card)
      (~> (deck)
          (if (send empty?) _ (send take))
          (==* (new board% [deck _]
                           [graveyard-stack graveyard-stack]
                           [lost-stack lost-stack])
               _)))))


(define initial-board
  (new board% [deck (list->stack all-cards)]))


(module+ examples
  (provide board-with-deck
           final-board)
  (define (board-with-deck cards)
    (new board% [deck (list->stack cards)]))
  (define final-board
    (board-with-deck '())))


(module+ board%-tests
  (require (submod ".." examples))
  (test-case
    "A card can be drawn"
    (~> ((board-with-deck '(a b c)))
        (send draw-card)
        (== (~> (is-a? board%) check-true)
            (check-equal? 'a))))
  (test-case
    "Multiple cards can be drawn"
    (~> ((board-with-deck '(a b c)))
        (feedback 2 (==* (send draw-card) _))
        (== (~> (is-a? board%) check-true)
            (check-equal? 'b)
            (check-equal? 'a))))
  (test-case
    "Drawing from an empty deck returns no cards"
    (~> (final-board)
        (send draw-card)
        (-< (~> count (check-equal? 1))
            (~> (is-a? board%) check-true)))))


(module+ test
  (require (submod ".." board%-tests)))
