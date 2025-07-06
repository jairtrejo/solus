#lang racket

(provide all-cards)


(require qi)
(require rackunit)
(require "qi-class.rkt")
(require "ui.rkt")


(define card
  (interface ()
             describe
             resolve))


(define-flow card? (is-a? card))


(module+ examples
  (provide dummy-card%)

  (define dummy-card%
    (class* object% (card)
      (super-new)

      (init-field name)

      (define/public (describe)
        ((ui) `(describe-card ,name)))

      (define/public (resolve the-board)
        ((ui) `(resolving-card ,name))
        the-board))))


(module+ card?-tests
  (require (submod ".." examples))
  (test-case
    "Returns true for a card"
    (~> ((new dummy-card% [name 'a]))
        card?
        check-true))
  (test-case
    "Returns false for a non-card"
    (~> (0)
        card?
        check-false)))


(define-flow roll->ship-damage
  (switch [(= 1) -3]
          [(= 2) -2]
          [(< 12) -1]
          [else 0]))


(define-flow roll->enemy-damage
  (switch [(= 20) 4]
          [(= 19) 3]
          [(= 18) 3]
          [(= 17) 2]
          [(= 16) 2]
          [(= 15) 2]
          [(> 5) 1]
          [else 0]))

(define enemy
  (interface () damage-in
                damage-out
                damage
                defeated?))


(define (ui-enemy-damaged damage remaining-life)
  ((ui) `(enemy-damaged #:damage ,damage #:life ,remaining-life)))


(define (ui-enemy-evaded)
  ((ui) '(enemy-evaded)))


(define enemy%
  (class object%
    (super-new)
    (init-field defense attack life)

    (define/public (damage-in roll)
      (~> (roll)
          (switch
            [(>= defense) roll->enemy-damage]
            [(<= attack) 0]
            [else 1])))
    
    (define/public (damage-out roll)
      (~> (roll)
          (switch
            [(<= attack) roll->ship-damage]
            [(>= defense) 0]
            [else -1])))

    (define/public (damage amount)
      (~> (amount)
          (- life _)
          (effect (if (gen (zero? amount))
                      (~> ground ui-enemy-evaded)
                      (ui-enemy-damaged amount _)))
          (clone #:life _)))

    (define/public (defeated?)
      (not (positive? life)))

    (define (clone #:defense [defense defense]
                   #:attack [attack attack]
                   #:life [life life])
      (new enemy% [defense defense]
                  [attack attack]
                  [life life]))))


(module+ enemy%-tests
  (require (submod "ui.rkt" examples))
  (define test-enemy (new enemy% [defense 14] [attack 5] [life 3]))
  (test-case
    "rolls above defense cause damage"
    (~> (test-enemy)
        (send damage-in 20)
        (check-equal? 4)))
  (test-case
    "rolls equal to defense cause damage"
    (~> (test-enemy)
        (send damage-in 14)
        (check-equal? 1)))
  (test-case
    "rolls below defense but above attack cause 1 damage"
    (~> (test-enemy)
        (send damage-in 10)
        (check-equal? 1)))
  (test-case
    "rolls equal to attack cause no damage"
    (~> (test-enemy)
        (send damage-in 5)
        (check-equal? 0)))
  (test-case
    "rolls bellow attack cause no damage"
    (~> (test-enemy)
        (send damage-in 1)
        (check-equal? 0)))
  (test-case
    "being damaged decreases life"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 2 #:life 1)))])
      (~> (test-enemy)
        (send damage 2)
        (get-field life _)
        (check-equal? 1))))
  (test-case
    "it is defeated after being damaged enough"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 4 #:life -1)))])
      (~> (test-enemy)
        (send damage 4)
        (send defeated?)
        check-true)))
  (test-case
    "damages ship when roll is below attack"
    (~> (test-enemy)
      (send damage-out 1)
      (check-equal? -3)))
  (test-case
    "damages ship when roll is equal to attack"
    (~> (test-enemy)
      (send damage-out 5)
      (check-equal? -1)))
  (test-case
    "only damages 1 when roll is above attack but below defense"
    (~> (test-enemy)
      (send damage-out 6)
      (check-equal? -1)))
  (test-case
    "does not damage ship when roll is equal to defense"
    (~> (test-enemy)
      (send damage-out 14)
      (check-equal? 0)))
  (test-case
    "does not damage ship when roll is above defense"
    (~> (test-enemy)
      (send damage-out 20)
      (check-equal? 0))))


(define (ui-user-roll)
  ((ui) '(roll d20)))


(define encounter-card
  (interface (card)))


(define encounter-card%
  (class* object% (encounter-card)
    (super-new)
    (abstract describe discard make-enemy)

    (define/public (warp board enemy)
      (~> (board)
          (send age-pilot 10)
          (send discard this #:stack 'lost)))

    (define/pubment (defeat board)
      ((ui) '(player-defeated))
      (inner board defeat board))

    (define/pubment (victory board)
      ((ui) '(enemy-defeated))
      (inner board victory board))

    (define/public (battle board enemy)
      (~> ((ui-user-roll))
          (-< (~>> (send enemy damage-out)
                   (send board damage-ship))
              (~>> (send enemy damage-in)
                   (send enemy damage)))
          (switch
            [(~> 1> (send game-over?)) (~>> 1> (send this defeat))]
            [(~> 2> (send defeated?)) (~>> 1> (send this victory))]
            [else _])
          (if (~> count (= 1))
              (send this discard _)
              _)))

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
  (require (submod "ui.rkt" examples))
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

;
; SPECIFIC CARDS
;

;; (define-card/encounter plasma-cruiser-card%
;;   #:name "Plasma cruiser"
;;   #:stats (#:defense 14 #:attack 5 #:life 3)
;;   #:text "Initiating warp drive causes you to age +30 years"
;;
;;   (when warp
;;     (send age-pilot 30)
;;
;;   #:discard graveyard)
  

(define plasma-cruiser-card%
  (class* encounter-card% (card)
    (super-new)

    (define/override (describe)
      ((ui) `(describe-card
               #:name "Plasma cruiser"
               #:text "Initiating warp drive causes you to age +30 years"
               #:defense 14 #:attack 5 #:life 3)))

    (define/override (make-enemy)
      (new enemy% [defense 14]
                  [attack 5]
                  [life 3]))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))

    (define/override (warp board enemy)
      (~> (board)
          (send age-pilot 30)
          (send discard this #:stack 'lost)))))


(module+ plasma-cruiser-card%-tests
  (require (submod ".." examples))
  (require (submod "ui.rkt" examples))
  (test-case
    "warping ages you 30 years"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (age-pilot years)
               (check-equal? years 30)
               (super age-pilot years)))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) warp]))])
      (~> ((new plasma-cruiser-card%))
          (send resolve test-board)
          (send game-over?)
          check-true))))

;; (define-card/encounter marauder-card%
;;   #:name "Marauder"
;;   #:text "You can't initiate warp drive."
;;   #:stats (#:defense 17 #:attack 8 #:life 2)
;;   
;;   #:disable-action warp
;;
;;   #:discard graveyard)
  

(define marauder-card%
  (class* encounter-card% (card)
    (super-new)

    (define/override (describe)
      ((ui) `(describe-card
               #:name "Marauder"
               #:text "You can't initiate warp drive."
               #:defense 17 #:attack 8 #:life 2)))

    (define/override (make-enemy)
      (new enemy% [defense 17]
                  [attack 8]
                  [life 2]))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))

    (define/override (available-actions)
      (~>> ((super available-actions))
           (remove 'warp)))))


(module+ marauder-card%-tests
  (require (submod ".." examples))
  (require (submod "ui.rkt" examples))
  (test-case
    "you can't warp away"
    (parameterize ([ui (test-ui '([(pick-action battle) battle]))])
      (~> ((new marauder-card%))
          (send available-actions)
          (check-equal? '(battle))))))

;; (define-card/encounter star-strider-card%
;;   #:name "Star strider"
;;   #:text "Defeating Star Strider causes you to age +20 years"
;;   #:stats (#:defense 12 #:attack 7 #:life 2)
;;
;;   (when victory
;;     (send age-pilot 20))
;;   
;;   #:discard graveyard)


(define star-strider-card%
  (class* encounter-card% (card)
    (super-new)

    (define/override (describe)
      ((ui) `(describe-card
               #:name "Star strider"
               #:text "Defeating Star Strider causes you to age +20 years."
               #:defense 12 #:attack 7 #:life 2)))

    (define/override (make-enemy)
      (new enemy% [defense 12]
                  [attack 7]
                  [life 2]))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))

    (define/augment (victory board)
      (send board age-pilot 20))))


(module+ star-strider-card%-tests
  (require (submod ".." examples))
  (require (submod "ui.rkt" examples))
  (test-case
    "winning ages you 20 years"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (age-pilot years)
               (check-equal? years 20)
               (super age-pilot years)))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-damaged #:damage 4 #:life -2)
                                  (enemy-defeated)))])
      (~> ((new star-strider-card%))
          (send resolve test-board)
          (send game-over?)
          check-false))))


(define all-cards
  (list (new plasma-cruiser-card%)
        (new marauder-card%)
        (new star-strider-card%)))


(module+ examples
  (provide pilot-killer-card%
           ship-destroyer-card%
           test-board%)

  (define pilot-killer-card%
    (class* dummy-card% (card)
      (super-new [name 'pilot-killer])

      (define/override (resolve the-board)
        ((ui) `(killing-pilot))
        (send the-board age-pilot 100))))

  (define ship-destroyer-card%
    (class* dummy-card% (card)
      (super-new [name 'ship-destroyer])

      (define/override (resolve the-board)
        ((ui) `(destroying-ship))
        (send the-board damage-ship -10))))
  ; Workaround for https://github.com/racket/racket/issues/1101
  (define test-board%
    (class object%
       (super-new)
       (init-field [aged #f]
                   [damaged #f]
                   [dead #f]
                   [destroyed #f])
       (define/public (age-pilot years)
         (cond
           [(> years 20) (clone #:aged #t #:dead #t)]
           [(> years 0) (clone #:aged #t)]
           [else this]))
       (define/public (damage-ship [damage -1])
         (cond
           [(< damage -2) (clone #:damaged #t #:destroyed #t)]
           [(< damage 0) (clone #:damaged #t)]
           [else this]))
       (define/public (pilot-aged?) aged)
       (define/public (ship-damaged?) damaged)
       (define/public (game-over?) (or dead destroyed))
       (define/public (discard c #:stack [destination #f]) this)
       (define (clone #:aged [aged aged]
                      #:damaged [damaged damaged]
                      #:dead [dead dead]
                      #:destroyed [destroyed destroyed])
         (new test-board% [aged aged]
                          [damaged damaged]
                          [dead dead]
                          [destroyed destroyed])))))

(module+ test
  (require (submod ".." card?-tests))
  (require (submod ".." enemy%-tests))
  (require (submod ".." encounter-card%-tests))
  (require (submod ".." plasma-cruiser-card%-tests))
  (require (submod ".." marauder-card%-tests))
  (require (submod ".." star-strider-card%-tests)))
