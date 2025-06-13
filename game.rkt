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
             (loop (yield))]))))


(define initial-board
  (board (list->stack all-cards)
         (list->stack '())
         (list->stack '())))


(define-flow draw-card
  (==* (~> (send take) X) _))


(define-flow choose-left?
  (gen (string=? "l" (ui '(choose-card)))))
  

(define-switch reveal-card
  (% (~> collect length) _)
  [(= 2) (if choose-left? _ X)]
  [(= 1) (effect (gen (ui '(auto-reveal))) _)]
  [(= 0) ground])


(define-flow main-loop
  (when (~> (block 1) live?)
        (~> (==* _ reveal-card)
            (group 2 (~> X (send _ resolve ui _)) _)
            (==* (update-deck (if (send empty?) _ draw-card))
                 _))))


(~> (initial-board)
    (update-deck (feedback 2 draw-card))
    (feedback (while live?) main-loop))
