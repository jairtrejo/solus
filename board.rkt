#lang racket


(provide board initial-board)


(require qi)
(require rackunit)
(require "stack.rkt")
(require "qi-class.rkt")
(require "card/all-cards.rkt")
(require "ui.rkt")


(define board
  (interface ()
             draw-card
             discard
             shuffle-into-deck
             age-pilot
             damage-ship))


(define (ui-pilot-aged years age)
  ((ui) `(pilot-aged #:years ,years #:age ,age)))

(define (ui-ship-damaged damage total-damage)
  ((ui) `(ship-damaged #:damage ,damage #:total-damage ,total-damage)))

(define (ui-ship-evaded)
  ((ui) '(ship-evaded)))


(define board%
  (class* object% (board)
    (super-new)

    (init-field [deck (list->stack '())]
                [graveyard (list->stack '())]
                [lost (list->stack '())]
                ;TODO Break out ship and pilot into their own classes
                [pilot-age 0]
                [ship-damage 0])

    (define/public (draw-card)
      (~> (deck)
          (if (send empty?) _ (send take))
          (==* (clone #:deck _) _)))

    (define/public (discard c #:stack [destination #f])
      ;TODO: Report discards to the UI
      (match destination
        ['graveyard (clone #:graveyard (send graveyard put c))]
        ['lost (clone #:lost (send lost put c))]
        [_ this]))

    (define/public (shuffle-into-deck #:stack source)
      ;TODO: Report shuffles to the UI
      (match source
        ['graveyard (clone #:deck (send deck shuffle-in graveyard)
                           #:graveyard (list->stack '()))]
        ['lost (clone #:deck (send deck shuffle-in lost)
                      #:lost (list->stack '()))]
        [(? stack? s) (clone #:deck (send deck shuffle-in s))]
        [c (clone #:deck (send deck shuffle-in (list->stack (list c))))]))

    (define/public (age-pilot [years 10])
      (~> (years)
          (+ pilot-age)
          (effect (ui-pilot-aged years _))
          (clone #:pilot-age _)))

    (define/public (damage-ship [damage -1])
      (~> (damage)
          (if zero?
              (~> (gen ship-damage)
                  (effect (~> ground ui-ship-evaded)))
              (~>> (+ ship-damage)
                   (effect (ui-ship-damaged damage _))))
          (clone #:ship-damage _)))

    (define/public (pilot-dead?)
      (> pilot-age 90))

    (define/public (ship-destroyed?)
      (< ship-damage -9))

    (define/public (game-over?)
      (~> (this)
          (or (send pilot-dead?)
              (send ship-destroyed?))))

    ;TODO: Write a define/clone macro
    (define (clone #:deck [deck deck]
                   #:graveyard [graveyard graveyard]
                   #:lost [lost lost]
                   #:pilot-age [pilot-age pilot-age]
                   #:ship-damage [ship-damage ship-damage])
      (new board% [deck deck]
                  [graveyard graveyard]
                  [lost lost]
                  [pilot-age pilot-age]
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
    "a single card can be shuffled into the deck"
    (~> (empty-board)
        (send shuffle-into-deck #:stack 'x)
        (~> (get-field deck _)
            stack->list
            (check-equal? '(x)))))
  (test-case
    "the pilot can be aged"
    (parameterize ([ui (test-ui `((pilot-aged #:years 10 #:age 10)))])
      (~> (initial-board)
          (send age-pilot)
          (get-field pilot-age _)
          (check-equal? 10))))
  (test-case
    "the pilot can be aged a specific amount"
    (parameterize ([ui (test-ui `((pilot-aged #:years 30 #:age 30)))])
      (~> (initial-board)
          (send age-pilot 30)
          (get-field pilot-age _)
          (check-equal? 30))))
  (test-case
    "the pilot can be aged multiple times"
    (parameterize ([ui (test-ui `((pilot-aged #:years 30 #:age 30)
                                  (pilot-aged #:years 10 #:age 40)))])
      (~> (initial-board)
          (send age-pilot 30)
          (send age-pilot)
          (get-field pilot-age _)
          (check-equal? 40))))
  (test-case
    "the pilot is not dead initially"
    (~> (initial-board)
        (send game-over?)
        check-false))
  (test-case
    "the pilot can be killed by aging them"
    (parameterize ([ui (test-ui `((pilot-aged #:years 100 #:age 100)))])
      (~> (initial-board)
          (send age-pilot 100)
          (send game-over?)
          check-true)))
  (test-case
    "the ship can be damaged"
    (parameterize ([ui (test-ui `((ship-damaged #:damage -1 #:total-damage -1)))])
      (~> (initial-board)
          (send damage-ship)
          (get-field ship-damage _)
          (check-equal? -1))))
  (test-case
    "the ship can be damaged a specific amount"
    (parameterize ([ui (test-ui `((ship-damaged #:damage -2 #:total-damage -2)))])
      (~> (initial-board)
          (send damage-ship -2)
          (get-field ship-damage _)
          (check-equal? -2))))
  (test-case
    "the ship can be damaged multiple times"
    (parameterize ([ui (test-ui `((ship-damaged #:damage -2 #:total-damage -2)
                                  (ship-damaged #:damage -1 #:total-damage -3)))])
      (~> (initial-board)
          (send damage-ship -2)
          (send damage-ship)
          (get-field ship-damage _)
          (check-equal? -3))))
  (test-case
    "the ship is not destroyed initially"
    (~> (initial-board)
        (send game-over?)
        check-false))
  (test-case
    "the ship can be damaged without destroying it"
    (parameterize ([ui (test-ui `((ship-damaged #:damage -1 #:total-damage -1)))])
      (~> (initial-board)
          (send damage-ship)
          (send game-over?)
          check-false)))
  (test-case
    "the ship can be destroyed by damaging it"
    (parameterize ([ui (test-ui `((ship-damaged #:damage -10 #:total-damage -10)))])
      (~> (initial-board)
          (send damage-ship -10)
          (send game-over?)
          check-true)))
  (test-case
    "the ship can evade damage"
    (parameterize ([ui (test-ui `((ship-evaded)))])
      (~> (initial-board)
          (send damage-ship 0)
          (send game-over?)
          check-false))))


(module+ test
  (require (submod ".." board%-tests)))
