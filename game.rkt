#lang racket

(require racket/generator)

(require qi)
(require rackunit)

(require "board.rkt")
(require "card.rkt")
(require "qi-class.rkt")
(require "stack.rkt")
(require "ui.rkt")


(define-qi-foreign-syntaxes check-match)


(define ui (make-parameter terminal-ui))


; TODO: Make board an object
(define initial-board
  ; TODO: Implement shuffle
  (board (list->stack all-cards)
         (list->stack '())
         (list->stack '())))


(define-flow draw-card
  (==* (send take) _))

(module+ draw-card-tests
  (test-case
    "can draw a single card"
    (~> ((list->stack '(a b c)))
        draw-card
        collect
        (check-match (list _ 'a))))
  (test-case
    "can draw multiple cards"
    (~> ((list->stack '(a b c)))
        draw-card
        draw-card
        collect
        (check-match (list _ 'b 'a))))
  (test-case
    "can be used in a feedback loop"
    (~> ((list->stack '(a b c)))
        (feedback 3 draw-card)
        collect
        (check-match (list _ 'c 'b 'a)))))


(define-flow chose-new?
  (gen (string=? "n" ((ui) '(choose-card)))))

(module+ chose-new?-tests
  (require (submod "ui.rkt" examples))
  (test-case
    "returns true when the user picks 'n'"
    (parameterize ([ui (test-ui `([(choose-card) "n"]))])
      (~> ((chose-new?))
          check-true)))
  (test-case
    "returns false when the user picks 'o"
    (parameterize ([ui (test-ui `([(choose-card) "o"]))])
      (~> ((chose-new?))
          check-false))))


(define-flow auto-reveal
  (effect (~> ground ((ui) '(auto-reveal)))))

(module+ auto-reveal-tests
  (require (submod "ui.rkt" examples))
  (test-case
    "passes through any values"
    (parameterize ([ui (test-ui `((auto-reveal)))])
      (~> (1 2 3)
          auto-reveal
          collect
          (check-equal? '(1 2 3))))))


(define-switch reveal-card
  (% count _)
  [(= 2) (if chose-new? _ X)]
  [(= 1) auto-reveal]
  [(= 0) ground])

(module+ reveal-card-tests
  (require (submod "ui.rkt" examples))
  (test-case
    "leaves cards alone when user chooses the new card"
    (parameterize ([ui (test-ui `([(choose-card) "n"]))])
      (~> ('a 'b)
          reveal-card
          collect
          (check-equal? '(a b)))))
  (test-case
    "re-arranges cards when user chooses the old card"
    (parameterize ([ui (test-ui `([(choose-card) "o"]))])
      (~> ('a 'b)
          reveal-card
          collect
          (check-equal? '(b a)))))
  (test-case
    "auto-reveals card when it's the only option"
    (parameterize ([ui (test-ui `((auto-reveal)))])
      (~> ('a)
          reveal-card
          (check-equal? 'a))))
  (test-case
    "returns no values when there are no options"
    (~> ()
        reveal-card
        live?
        check-false)))


(define-flow main-loop
  (when (~> (block 1) live?)
        (~> (==* _ reveal-card)
            (group 2 (~> (== _ (as current-card))
                         (~>> (send current-card resolve (ui))
                              ; TODO Implement discard
                              ;; (send current-card discard ui)
                              (update-deck (if (send empty?) _ draw-card))))
                     _))))

(module+ main-loop-tests
  (require (submod "ui.rkt" examples))
  (define test-board
    (board (list->stack (list (new dummy-card% [name 'c])))
           (list->stack '())
           (list->stack '())))
  (test-case
    "reveals the new card, resolves it and replaces it from the deck"
    (parameterize ([ui (test-ui `([(choose-card) "n"]
                                  (resolving-card a)))])
      (~> (test-board (new dummy-card% [name 'a]) (new dummy-card% [name 'b]))
          main-loop
          (==* (~> board-deck
                   (send empty?)
                   check-true)
               (~> (>< (get-field name _))
                   collect
                   (check-equal? '(c b)))))))
  (test-case
    "reveals the old card, resolves it and replaces it from the deck"
    (parameterize ([ui (test-ui `([(choose-card) "o"]
                                  (resolving-card b)))])
      (~> (test-board (new dummy-card% [name 'a]) (new dummy-card% [name 'b]))
          main-loop
          (==* (~> board-deck
                   (send empty?)
                   check-true)
               (~> (>< (get-field name _))
                   collect
                   (check-equal? '(c a)))))))
  (test-case
    "returns no values when there are no cards to reveal"
    (~> (test-board)
        main-loop
        live?
        check-false))
  (test-case
    "does not replace card when deck is empty"
    (define empty-deck-board
      (board (list->stack '())
             (list->stack '())
             (list->stack '())))
    (parameterize ([ui (test-ui `((auto-reveal)
                                  (resolving-card c)))])
      (~> (empty-deck-board (new dummy-card% [name 'c]))
          main-loop
          count
          (check-equal? 1)))))


(module+ main
  (~> (initial-board)
      (update-deck (feedback 2 draw-card))
      (feedback (while live?) main-loop)
      (effect ((ui) '(game-over)))))


(module+ test
  (require (submod ".." draw-card-tests))
  (require (submod ".." chose-new?-tests))
  (require (submod ".." auto-reveal-tests))
  (require (submod ".." reveal-card-tests))
  (require (submod ".." main-loop-tests)))
