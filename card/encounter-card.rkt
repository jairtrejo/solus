#lang racket

(provide encounter-card%)

(require qi)
(require rackunit)
(require "card.rkt")
(require "enemy.rkt")
(require "ui.rkt")

(define encounter-card
  (interface (card)))


;TODO Write tests for discard behavior

(define ui-player-defeated
  ((ui) '(player-defeated)))

(define ui-enemy-defeated
  ((ui) '(enemy-defeated)))

(define ui-enemy-escaped
  ((ui) '(enemy-escaped)))

(define encounter-card%
  (class* object% (encounter-card)
    (super-new)
    (abstract describe discard make-enemy)

    (define/pubment (on-defeat board)
      (ui-player-defeated)
      (inner board on-defeat board))

    (define/pubment (on-victory board)
      (ui-enemy-defeated)
      (inner board on-victory board))

    (define/pubment (on-escape board)
      (ui-enemy-escaped)
      (inner board on-escape board))

    (define/public (battle board enemy)
      (~> ((ui-user-roll))
          (send enemy handle-roll _ board)
          (switch
            [(~> 2> (not live?)) _]
            [(~> 1> (send game-over?)) (~>> 1> (send this defeat))]
            [(~> 2> (send defeated?)) (~>> 1> (send this victory))])))

    ;TODO: Refactor so we don't require a method definition
    (define/pubment (warp-effect board)
      (inner (send board age-pilot 10)
             warp-effect board))

    (define/public (warp board enemy)
      (~> (board)
          (send this warp-effect _)
          (send discard this #:stack 'lost)))

    (define/pubment (thwart-effect board)
      (inner board thwart-effect board))

    (define/public (thwart board enemy)
      (~>> (board)
           (send this thwart-effect)
           (send this discard)))

    (define/public (contest goal board enemy)
      (ui-announce-contest goal)
      (define win?
        (~> (0 0)
            (feedback (while (all (< goal)))
                      (if (~>> (gen (ui-user-roll))
                               (send enemy damage-in)
                               positive?)
                          (~> (effect (~> ground ui-contest-success))
                              (== add1 _))
                          (~> (effect (~> ground ui-contest-failure))
                              (== _ add1))))
            >))
      (if win?
          (send this victory board)
          (send this defeat board)))

    (define/public (available-actions)
      (list 'battle 'warp))

    (define/public (ui-pick-action)
      ((ui) `(pick-action ,@(available-actions))))

    (define/public (resolve board)
      (~> (board (make-enemy))
          (feedback
            (while (~> count (> 1)))
            (~> (effect (~> ground
                            (gen (send this ui-pick-action))
                            (as user-action)))
                (dynamic-send this user-action _ _)))))))


(module+ examples
  (provide dummy-encounter-card%)

  (define dummy-encounter-card%
    (class encounter-card%
      (super-new)

      (init-field name)

      (define/override (describe)
        ((ui) `(describe-card ,name)))

      (define/override (discard the-board)
        (send the-board discard this))

      (define/override (make-enemy)
        (new enemy% [defense 14]
                    [attack 5]
                    [life 3])))))


(module+ encounter-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (define test-board (new test-board%))
  (define test-card (new dummy-encounter-card% [name 'enemy-ship]))
  (define test-enemy (new enemy% [defense 14] [attack 5] [life 3]))
  (define-flow check-enemy-gone (~> count (check-equal? 1)))
  (test-case
    "warping ages the pilot 10 years and removes the enemy"
    (~> (test-card)
        (send warp test-board test-enemy)
        (effect check-enemy-gone)
        (send pilot-aged?)
        check-true))
  (test-case
    "battling can damage the enemy"
    (parameterize ([ui (test-ui '([(roll d20) 20]
                                  (enemy-damaged #:damage 4 #:life -1)
                                  (enemy-defeated)))])
      (~> (test-card)
          (send battle test-board test-enemy)
          (effect check-enemy-gone)
          (send game-over?)
          check-false)))
  (test-case
    "battling can destroy the ship"
    (parameterize ([ui (test-ui `([(roll d20) 1]
                                  (enemy-evaded)
                                  (player-defeated)))])
      (~> (test-card)
          (send battle test-board test-enemy)
          (effect check-enemy-gone)
          (send game-over?)
          check-true)))
  (test-case
    "thwarting discards the enemy immediately"
    (~> (test-card)
        (send thwart test-board test-enemy)
        (effect check-enemy-gone)
        (send game-over?)
        check-false))
  (test-case
    "a contest ends when goal is reached by player"
    (parameterize ([ui (test-ui `((contest #:goal 2)
                                  [(roll d20) 20]
                                  (contest-success)
                                  [(roll d20) 20]
                                  (contest-success)
                                  (enemy-defeated)))])
      (~> (test-card)
          (send contest 2 test-board test-enemy)
          (effect check-enemy-gone)
          (send game-over?)
          check-false)))
  (test-case
    "a contest ends when goal is reached by enemy"
    (parameterize ([ui (test-ui `((contest #:goal 2)
                                  [(roll d20) 1]
                                  (contest-failure)
                                  [(roll d20) 1]
                                  (contest-failure)
                                  (player-defeated)))])
      (~> (test-card)
          (send contest 2 test-board test-enemy)
          (effect check-enemy-gone)
          (send game-over?)
          check-false)))
  (test-case
    "resolving lets player warp away at the beginning"
    (parameterize ([ui (test-ui `([(pick-action battle warp) warp]))])
      (~> (test-card)
          (send resolve test-board)
          (send game-over?)
          check-false)))
  (test-case
    "resolving lets player warp away in the middle of the encounter"
    (parameterize ([ui (test-ui `([(pick-action battle warp) battle]
                                  [(roll d20) 17]
                                  (enemy-damaged #:damage 2 #:life 1)
                                  [(pick-action battle warp) warp]))])
      (~> (test-card)
          (send resolve test-board)
          (send game-over?)
          check-false)))
  (test-case
    "resolving ends when enemy is destroyed"
    (parameterize ([ui (test-ui `([(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-damaged #:damage 4 #:life -1)
                                  (enemy-defeated)))])
      (~> (test-card)
          (send resolve test-board)
          (send game-over?)
          check-false)))
  (test-case
    "resolving ends when ship is destroyed"
    (parameterize ([ui (test-ui `([(pick-action battle warp) battle]
                                  [(roll d20) 1]
                                  (enemy-evaded)
                                  (player-defeated)))])
      (~> (test-card)
          (send resolve test-board)
          (send game-over?)
          check-true))))

