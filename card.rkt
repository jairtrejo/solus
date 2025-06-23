#lang racket

(provide all-cards)


(require qi)
(require rackunit)
(require "qi-class.rkt")
(require "ui.rkt")


(define card
  (interface ()
             resolve
             discard))


(define-flow card? (is-a? card))


(module+ examples
  (provide dummy-card%)

  (define dummy-card%
    (class* object% (card)
      (super-new)

      (init-field name)

      (define/public (resolve the-board)
        ((ui) `(resolving-card ,name))
        the-board)

      (define/public (discard the-board)
        ((ui) `(discarding graveyard ,name))
        (send the-board discard this #:stack 'graveyard)))))


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
                defeat
                defeated?))


(define (ui-enemy-damaged damage remaining-life)
  ((ui) `(enemy-damaged #:damage ,damage #:life ,remaining-life)))


(define (ui-enemy-evaded)
  ((ui) '(enemy-evaded)))


(define enemy%
  (class object%
    (super-new)
    (init-field defense attack life)
    
    ;TODO: Separate into damage-in (calculation) and damage (imperative)
    (define/public (damage-in roll)
      (~> (roll)
          (switch
            [(>= defense) roll->enemy-damage]
            [(<= attack) 0]
            [else 1])
          (if zero?
              (~> (gen life)
                  (effect (~> ground ui-enemy-evaded)))
              (~>> (effect (as damage))
                   (- life)
                   (effect (ui-enemy-damaged damage _))))
          (clone #:life _)))
    
    (define/public (damage-out roll)
      (~> (roll)
          (switch
            [(<= attack) roll->ship-damage]
            [(>= defense) 0]
            [else -1])))

    (define/public (defeat)
      (clone #:life 0))

    (define/public (defeated?)
      (< life 1))

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
    "can be damaged by rolls above defense"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 4 #:life -1)))])
      (~> (test-enemy)
          (send damage-in 20)
          (get-field life _)
          (check-equal? -1))))
  (test-case
    "can be damaged by rolls equal to defense"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 1 #:life 2)))])
      (~> (test-enemy)
          (send damage-in 14)
          (get-field life _)
          (check-equal? 2))))
  (test-case
    "is damaged 1 in rolls below defense but above attack"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 1 #:life 2)))])
      (~> (test-enemy)
        (send damage-in 13)
        (get-field life _)
        (check-equal? 2))))
  (test-case
    "is not damaged in rolls equal to attack"
    (parameterize ([ui (test-ui '((enemy-evaded)))])
      (~> (test-enemy)
        (send damage-in 5)
        (get-field life _)
        (check-equal? 3))))
  (test-case
    "is not damaged in rolls below to attack"
    (parameterize ([ui (test-ui '((enemy-evaded)))])
      (~> (test-enemy)
        (send damage-in 4)
        (get-field life _)
        (check-equal? 3))))
  (test-case
    "it is defeated after being damaged enough"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 4 #:life -1)))])
      (~> (test-enemy)
        (send damage-in 20)
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


(define (user-roll)
  ((ui) '(roll d20)))


(define encounter-card
  (interface (card)
             make-enemy
             pick-action))


(define encounter-card%
  (class* object% (encounter-card)
    (super-new)
    (abstract discard pick-action make-enemy)

    (define/public (warp board enemy)
      (~> (board enemy)
          (== (send age-pilot 10) (send defeat))))

    (define/public (battle board enemy)
      (~> ((user-roll))
          (-< (~>> (send enemy damage-out)
                   (send board damage-ship))
              (send enemy damage-in _))))

    (define/public (resolve board)
      (~> (board (make-enemy))
          (feedback
            (while (~> (== (-< (send pilot-dead?)
                               (send ship-destroyed?))
                           (send defeated?))
                       none?))
            (~>> (-< (gen (send this pick-action)) _)
                 (dynamic-send this)))
          (select 1)))))


(module+ examples
  (provide dummy-encounter-card%)

  (define dummy-encounter-card%
    (class encounter-card%
      (super-new)

      (init-field name)

      (define/override (discard the-board)
        (send the-board discard this))

      (define available-actions
        (list 'battle
              'warp))

      (define/override (pick-action)
        ((ui) `(pick-action ,@available-actions)))

      (define/override (make-enemy)
        (new enemy% [defense 14]
                    [attack 5]
                    [life 3])))))


(module+ encounter-card%-tests
  (require (submod ".." examples))
  (require (submod "ui.rkt" examples))
  ; Workaround for https://github.com/racket/racket/issues/1101
  (define test-board
    (new (class object%
           (super-new)
           (init-field [dead #f] [destroyed #f])
           (define/public (age-pilot years)
             (if (> years 0)
                 (new this% [dead #t] [destroyed destroyed])
                 this))
           (define/public (pilot-dead?)
             dead)
           (define/public (damage-ship [damage -1])
             (if (< damage 0)
                 (new this% [dead dead] [destroyed #t])
                 this))
           (define/public (ship-destroyed?)
             destroyed))))
  (define test-card (new dummy-encounter-card% [name 'enemy-ship]))
  (define test-enemy (new enemy% [defense 14] [attack 5] [life 3]))
  (test-case
    "warping ages the pilot 10 years and defeats the enemy"
    (~> (test-card)
        (send warp test-board test-enemy)
        (== (send pilot-dead?) (send defeated?))
        all?
        check-true))
  (test-case
    "battling can defeat the enemy"
    (parameterize ([ui (test-ui '([(roll d20) 20]
                                  (enemy-damaged #:damage 4 #:life -1)))])
      (~> (test-card)
          (send battle test-board test-enemy)
          2>
          (send defeated?)
          check-true)))
  (test-case
    "battling can destroy the ship"
    (parameterize ([ui (test-ui `([(roll d20) 1]
                                  (enemy-evaded)))])
      (~> (test-card)
          (send battle test-board test-enemy)
          1>
          (send ship-destroyed?)
          check-true)))
  (test-case
    "resolving lets player pick actions until victory"
    (parameterize ([ui (test-ui `([(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-damaged #:damage 4 #:life -1)))])
      (~> (test-card)
          (send resolve test-board)
          1>
          (send ship-destroyed?)
          check-false)))
  (test-case
    "resolving lets player pick actions until defeat"
    (parameterize ([ui (test-ui `([(pick-action battle warp) battle]
                                  [(roll d20) 1]
                                  (enemy-evaded)))])
      (~> (test-card)
          (send resolve test-board)
          1>
          (send ship-destroyed?)
          check-true)))
  (test-case
    "resolving lets player warp away at the beginning"
    (parameterize ([ui (test-ui `([(pick-action battle warp) warp]))])
      (~> (test-card)
          (send resolve test-board)
          1>
          (send ship-destroyed?)
          check-false)))
  (test-case
    "resolving lets player warp away in the middle of the encounter"
    (parameterize ([ui (test-ui `([(pick-action battle warp) battle]
                                  [(roll d20) 17]
                                  (enemy-damaged #:damage 2 #:life 1)
                                  [(pick-action battle warp) warp]))])
      (~> (test-card)
          (send resolve test-board)
          (send ship-destroyed?)
          check-false))))



(define (announce-card . card)
  ((ui) `(announce-card ,@card)))
  

;TODO: Write tests for plasma cruiser
(define plasma-cruiser-card%
  (class* encounter-card% (card)
    (super-new)

    (define/override (make-enemy)
      (new enemy% [defense 14]
                  [attack 5]
                  [life 3]))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))

    (define/override (warp board enemy)
      (~> (board enemy)
          (== (send age-pilot 30) (send defeat))))

    (define available-actions
      (list 'battle
            'warp))

    ;TODO Make available-actions a method and move this to encounter-card%
    (define/override (pick-action)
      ((ui) `(pick-action ,@available-actions)))))


;; (define-card/encounter plasma-cruiser-card%
;;   (stats #:defense 14 #:attack 5 #:life 3)
;;
;;   (when warp
;;     (== (send age-pilot 30) (send defeat)))
;;
;;   #:discard graveyard)


(define all-cards
  (list (new plasma-cruiser-card%)))


(module+ examples
  (provide pilot-killer-card%
           ship-destroyer-card%)

  (define pilot-killer-card%
    (class* object% (card)
      (super-new)

      (define/public (resolve the-board)
        ((ui) `(killing-pilot))
        (send the-board age-pilot 100))

      (define/public (discard the-board)
        ((ui) `(discarding void pilot-killer))
        the-board)))

  (define ship-destroyer-card%
    (class* object% (card)
      (super-new)

      (define/public (resolve the-board)
        ((ui) `(destroying-ship))
        (send the-board damage-ship -10))

      (define/public (discard the-board)
        ((ui) `(discarding void ship-destroyer))
        the-board))))


(module+ test
  (require (submod ".." card?-tests))
  (require (submod ".." enemy%-tests))
  (require (submod ".." encounter-card%-tests)))
