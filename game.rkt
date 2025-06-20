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
            (group 2 (~> (== _ (as revealed-card))
                         (~>> (send revealed-card resolve)
                              (send revealed-card discard)
                              (send _ draw-card)))
                     _))))

(module+ main-loop-tests
  (require (submod "ui.rkt" examples))
  (require (submod "board.rkt" examples))
  (define test-board (board-with-deck (list (new dummy-card% [name 'c]))))
  (test-case
    "reveals the new card, resolves it, discards it and replaces it from the deck"
    (parameterize ([ui (test-ui `([(choose-card) "n"]
                                  (resolving-card a)
                                  (discarding graveyard a)))])
      (~> (test-board (new dummy-card% [name 'a]) (new dummy-card% [name 'b]))
          main-loop
          (block 1)
          (~> (>< (get-field name _))
              collect
              (check-equal? '(c b))))))
  (test-case
    "reveals the old card, resolves it and replaces it from the deck"
    (parameterize ([ui (test-ui `([(choose-card) "o"]
                                  (resolving-card b)
                                  (discarding graveyard b)))])
      (~> (test-board (new dummy-card% [name 'a]) (new dummy-card% [name 'b]))
          main-loop
          (block 1)
          (~> (>< (get-field name _))
              collect
              (check-equal? '(c a))))))
  (test-case
    "returns no values when there are no cards to reveal"
    (~> (test-board)
        main-loop
        live?
        check-false))
  (test-case
    "does not replace card when deck is empty"
    (parameterize ([ui (test-ui `((auto-reveal)
                                  (resolving-card c)
                                  (discarding graveyard c)))])
      (~> (empty-board (new dummy-card% [name 'c]))
          main-loop
          count
          (check-equal? 1)))))


(module+ main
  (~> (initial-board)
      (feedback 2 (==* (send draw-card) _))
      (feedback (while live?) main-loop)
      (effect ((ui) '(game-over)))))


(module+ test
  (require (submod ".." chose-new?-tests))
  (require (submod ".." auto-reveal-tests))
  (require (submod ".." reveal-card-tests))
  (require (submod ".." main-loop-tests)))
