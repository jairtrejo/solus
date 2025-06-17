#lang racket


(provide board initial-board)


(require qi)
(require rackunit)
(require "stack.rkt")
(require "qi-class.rkt")
(require "card.rkt")


(define board
  (interface ()
             draw-card
             discard
             shuffle-into-deck))


(define board%
  (class* object% (board)
    (super-new)

    (init-field [deck (list->stack '())]
                [graveyard (list->stack '())]
                [lost (list->stack '())])

    (define/public (draw-card)
      (~> (deck)
          (if (send empty?) _ (send take))
          (==* (new board% [deck _]
                           [graveyard graveyard]
                           [lost lost])
               _)))

    (define/public (discard c #:stack [destination #f])
      (match destination
        ['graveyard (new board% [deck deck]
                                [lost lost]
                                [graveyard (send graveyard put c)])]
        ['lost (new board% [deck deck]
                           [graveyard graveyard]
                           [lost (send lost put c)])]
        [_ this]))

    (define/public (shuffle-into-deck #:stack source)
      (match source
        ['graveyard (new board% [deck (send deck shuffle-in graveyard)]
                                [lost lost]
                                [graveyard (list->stack '())])]
        ['lost (new board% [deck (send deck shuffle-in lost)]
                           [graveyard graveyard]
                           [lost (list->stack '())])]
        [(? stack? s) (new board% [deck (send deck shuffle-in s)]
                                  [graveyard graveyard]
                                  [lost lost])]
        [_ this]))))


(define initial-board
  (new board% [deck (list->stack all-cards)]))


(module+ examples
  (provide board-with-deck
           empty-board)
  (define (board-with-deck cards)
    (new board% [deck (list->stack cards)]))
  (define empty-board
    (board-with-deck '())))


(module+ board%-tests
  (require (submod ".." examples))
  (test-case
    "a card can be drawn"
    (~> ((board-with-deck '(a b c)))
        (send draw-card)
        (== (~> (is-a? board%) check-true)
            (check-equal? 'a))))
  (test-case
    "multiple cards can be drawn"
    (~> ((board-with-deck '(a b c)))
        (feedback 2 (==* (send draw-card) _))
        (== (~> (is-a? board%) check-true)
            (check-equal? 'b)
            (check-equal? 'a))))
  (test-case
    "drawing from an empty deck returns no cards"
    (~> (empty-board)
        (send draw-card)
        (-< (~> count (check-equal? 1))
            (~> (is-a? board%) check-true))))
  (test-case
    "a card can be discarded to the graveyard"
    (~> (empty-board)
        (send discard 'a #:stack 'graveyard)
        (get-field graveyard _)
        (send take)
        (block 1)
        (check-equal? 'a)))
  (test-case
    "a card can be discarded to the lost stack"
    (~> (empty-board)
        (send discard 'a #:stack 'lost)
        (get-field lost _)
        (send take)
        (block 1)
        (check-equal? 'a)))
  (test-case
    "a card can be removed from the game"
    (~> (initial-board)
        (send discard 'a)
        (-< (get-field lost _) (get-field graveyard _))
        (>< (~> (send empty?)
                check-true))))
  (test-case
    "the lost stack can be shuffled into the deck"
    (~> ((new board% [deck (list->stack '())]
                     [lost (list->stack '(a))]))
        (send shuffle-into-deck #:stack 'lost)
        (-< (~> (get-field deck _)
                stack->list
                (check-equal? '(a)))
            (~> (get-field lost _)
                (send empty?)
                check-true))))
  (test-case
    "the graveyard stack can be shuffled into the deck"
    (~> ((new board% [deck (list->stack '())]
                     [graveyard (list->stack '(a))]))
        (send shuffle-into-deck #:stack 'graveyard)
        (-< (~> (get-field deck _)
                stack->list
                (check-equal? '(a)))
            (~> (get-field graveyard _)
                (send empty?)
                check-true))))
  (test-case
    "an arbitrary stack can be shuffled into the deck"
    (~> (empty-board)
        (send shuffle-into-deck #:stack (list->stack '(x)))
        (~> (get-field deck _)
            stack->list
            (check-equal? '(x))))))


(module+ test
  (require (submod ".." board%-tests)))
