#lang racket

(require qi)
(require racket/generator)
(require "qi-class.rkt")
(require "stack.rkt")
(require "card.rkt")
(require "board.rkt")

(define ui
  (generator (msg)
    (displayln "Solus lost")
    (let loop ([msg msg])
      (begin
        (match msg
          ['(quit) (displayln "Game over")]
          ['(roll d20) (displayln "Press enter to roll a d20")
                       (read-string 1)
                       (define d20 (add1 (random 20)))
                       (displayln (~a "You rolled: " d20))
                       (loop (yield d20))]
          ['(choose-card) (displayln "Choose a card: (l)eft or (r)ight")
                          (define choice
                            (let loop ()
                              (define choice (string-trim (read-line)))
                              (if (member choice '("l" "r")) choice (loop))))
                          (loop (yield choice))]
          [msg (displayln msg)
               (loop (yield))])))))

(define (main)
  (define-values (left-card right-card deck)
    (~> ((list->stack all-cards))
        (send take)
        (== _ (send take))))
  (define lost-stack (list->stack '()))
  (define graveyard (list->stack '()))

  (define initial-board
    (board deck left-card right-card lost-stack graveyard))

  (on (initial-board)
    (feedback
      (while live?)
      (~> (-< (~> (gen (ui '(choose-card)))
                  (as choice))
              (if (gen (string=? choice "l"))
                  board-left-card
                  board-right-card)
              _)
          (send _ resolve ui _)
          (unless (~> board-deck (send empty?))
                  (~> (-< _ (~> board-deck (send take)))
                      (if (gen (string=? choice "l"))
                          (λ (s c d)
                            (struct-copy board s [left-card c]
                                                 [deck d]))
                          (λ (s c d)
                            (struct-copy board s [right-card c]
                                                 [deck d]))))))))

  (ui '(quit)))
 
(main)
