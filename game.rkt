#lang racket

(require qi)

(require "board.rkt")
(require "card.rkt")
(require "qi-class.rkt")
(require "stack.rkt")
(require "ui.rkt")


(define initial-board
  ; TODO: Implement shuffle
  (board (list->stack all-cards)
         (list->stack '())
         (list->stack '())))


(define-flow draw-card
  ; TODO: Change order of return values for objects
  (==* (~> (send take) X) _))


(define-flow choose-left?
  ; TODO: Rename to choose-new?
  (gen (string=? "l" (ui '(choose-card)))))


(define-flow auto-reveal
  (effect (esc (λ (_) (ui '(auto-reveal)))) _))


(define-switch reveal-card
  (% count _)
  [(= 2) (if choose-left? _ X)]
  [(= 1) auto-reveal]
  [(= 0) ground])


(define-flow main-loop
  (when (~> (block 1) live?)
        (~> (==* _ reveal-card)
            (group 2 (~> (== _ (as current-card))
                         (~>> (send current-card resolve ui)
                              ; TODO Implement discard
                              ;; (send current-card discard ui)
                              (update-deck (if (send empty?) _ draw-card))))
                     _))))


(define (main)
  (~> (initial-board)
      (update-deck (feedback 2 draw-card))
      (feedback (while live?) main-loop)
      (esc (λ () (ui '(game-over))))))

(main)
