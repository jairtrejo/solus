#lang racket


(provide board initial-board)


(require qi)
(require rackunit)
(require "stack.rkt")
(require "qi-class.rkt")
(require "card.rkt")
(require "ui.rkt")


(define board
  (interface ()
             draw-card
             discard
             shuffle-into-deck
             age-player
             damage-ship))


(define (ship-damage-changed ship-damage)
  ((ui) `(ship-damage-changed ,ship-damage)))


(define (player-age-changed player-age)
  ((ui) `(player-age-changed ,player-age)))


(define board%
  (class* object% (board)
    (super-new)

    (init-field [deck (list->stack '())]
                [graveyard (list->stack '())]
                [lost (list->stack '())]
                [player-age 0]
                [ship-damage 0])

    (define/public (draw-card)
      (~> (deck)
          (if (send empty?) _ (send take))
          (==* (clone #:deck _) _)))

    (define/public (discard c #:stack [destination #f])
      (match destination
        ['graveyard (clone #:graveyard (send graveyard put c))]
        ['lost (clone #:lost (send lost put c))]
        [_ this]))

    (define/public (shuffle-into-deck #:stack source)
      (match source
        ['graveyard (clone #:deck (send deck shuffle-in graveyard)
                           #:graveyard (list->stack '()))]
        ['lost (clone #:deck (send deck shuffle-in lost)
                      #:lost (list->stack '()))]
        [(? stack? s) (clone #:deck (send deck shuffle-in s))]
        [_ this]))

    (define/public (age-player [years 10])
      (~> (player-age)
          (+ years)
          (-< (effect player-age-changed ground)
              (clone #:player-age _))))

    (define/public (damage-ship [damage -1])
      (~> (ship-damage)
          (+ damage)
          (-< (effect ship-damage-changed ground)
              (clone #:ship-damage _))))

    (define (clone #:deck [deck deck]
                   #:graveyard [graveyard graveyard]
                   #:lost [lost lost]
                   #:player-age [player-age player-age]
                   #:ship-damage [ship-damage ship-damage])
      (new board% [deck deck]
                  [graveyard graveyard]
                  [lost lost]
                  [player-age player-age]
                  [ship-damage ship-damage]))))


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
  (require (submod "ui.rkt" examples))
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
            (check-equal? '(x)))))
  (test-case
    "the player can be aged"
    (parameterize ([ui (test-ui `((player-age-changed 10)))])
      (~> (initial-board)
          (send age-player)
          (get-field player-age _)
          (check-equal? 10))))
  (test-case
    "the player can be aged a specific amount"
    (parameterize ([ui (test-ui `((player-age-changed 30)))])
      (~> (initial-board)
          (send age-player 30)
          (get-field player-age _)
          (check-equal? 30))))
  (test-case
    "the ship can be damaged"
    (parameterize ([ui (test-ui `((ship-damage-changed -1)))])
      (~> (initial-board)
          (send damage-ship)
          (get-field ship-damage _)
          (check-equal? -1))))
  (test-case
    "the ship can be damaged a specific amount"
    (parameterize ([ui (test-ui `((ship-damage-changed -2)))])
      (~> (initial-board)
          (send damage-ship -2)
          (get-field ship-damage _)
          (check-equal? -2)))))


(module+ test
  (require (submod ".." board%-tests)))
