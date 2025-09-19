#lang racket

(provide all-cards)


(require qi)
(require rackunit)
(require "../qi-class.rkt")
(require "../ui.rkt")
(require "../stack.rkt")
(require "card.rkt")
(require "enemy.rkt")

(module+ examples
  (require (submod "./card.rkt" examples))
  (provide dummy-card%))


(define (ui-user-roll)
  ((ui) '(roll d20)))

(define (ui-player-defeated)
  ((ui) '(player-defeated)))

(define (ui-enemy-defeated)
  ((ui) '(enemy-defeated)))

(define (ui-announce-contest goal)
  ((ui) `(contest #:goal ,goal)))

(define (ui-contest-success)
  ((ui) '(contest-success)))

(define (ui-contest-failure)
  ((ui) '(contest-failure)))

;
; SPECIFIC CARDS
;

;; (define-card/encounter plasma-cruiser-card%
;;   #:name "Plasma cruiser"
;;   #:stats (#:defense 14 #:attack 5 #:life 3)
;;   #:text "Initiating warp drive causes you to age +30 years"
;;
;;   (when warp-effect
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

    (define/augment (warp-effect board)
      (send board age-pilot 30))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))))


(module+ plasma-cruiser-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
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
;;   (disable-action warp)
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
  (require (submod "../ui.rkt" examples))
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
;;     (send board age-pilot 20))
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
  (require (submod "../ui.rkt" examples))
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

;; (define-card/encounter nebula-raider-card%
;;   #:name "Nebula raider"
;;   #:text "Roll 5 or less: take -2 damage."
;;   #:stats (#:defense 12 #:attack 9 #:life 3)
;;
;;   (special-rolls
;;     [(< roll 5) -2])
;;
;;   #:discard graveyard)


(define nebula-raider-card%
  (class* encounter-card% (card)
    (super-new)

    (define/override (describe)
      ((ui) `(describe-card
               #:name "Nebula raider"
               #:text "Roll 5 or less: take -2 damage."
               #:defense 12 #:attack 9 #:life 3)))

    (define/override (make-enemy)
      (new
        (class enemy%
          (super-new)
          (define/override (special-rolls roll)
            (cond
              [(<= roll 5) -2]
              [else #f])))
        [defense 12] [attack 9] [life 3]))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))))


(module+ nebula-raider-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (test-case
    "rolling 5 or less causes -2 damage"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (damage-ship damage)
               (check-equal? damage -2)
               (super damage-ship damage)))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 5]
                                  (enemy-evaded)
                                  [(pick-action battle warp) warp]))])
      (~> ((new nebula-raider-card%))
          (send resolve test-board)
          (send game-over?)
          check-false)))
  (test-case
    "rolling 6 or more causes normal damage"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (damage-ship damage)
               (check-equal? damage -1)
               (super damage-ship damage)))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 6]
                                  (enemy-evaded)
                                  [(pick-action battle warp) warp]))])
      (~> ((new nebula-raider-card%))
          (send resolve test-board)
          (send game-over?)
          check-false))))


;; (define-card/encounter infestation-card%
;;   #:name "Infestation"
;;   #:text "You may roll. Every loss is -2 damage. Or you may take -4 damage to rid of the infestation immediately."
;;   #:stats (#:defense 6 #:life 2)
;;
;;   (special-rolls
;;     [miss? -2])
;;
;;   (enable-action thwart)
;;
;;   (when thwart
;;     (send board damage-ship -4))
;;
;;   #:discard lost)

(define infestation-card%
  (class encounter-card%
    (super-new)
    
    (define/override (describe)
      ((ui) `(describe-card
               #:name "Infestation"
               #:text "You may roll. Every loss is -2 damage. Or you may take -4 damage to rid of the infestation immediately."
               #:defense 6 #:life 2)))

    (define/override (make-enemy)
      (new
        (class enemy%
          (super-new)
          (define/override (special-rolls roll)
            (cond
              [(< roll 6) -2]
              [else #f])))
        [defense 6] [attack 6] [life 2]))

    (define/augment (thwart-effect board)
      (send board damage-ship -4))

    (define/override (available-actions)
      (~>> ((super available-actions))
           (cons 'thwart)))

    (define/override (discard board)
      (send board discard this #:stack 'lost))))
 
(module+ infestation-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (test-case
    "missing causes -2 damage"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (damage-ship damage)
               (check-equal? damage -2)
               (super damage-ship damage)))))
    (parameterize ([ui (test-ui '([(pick-action thwart battle warp) battle]
                                  [(roll d20) 5]
                                  (enemy-evaded)
                                  [(pick-action thwart battle warp) warp]))])
      (~> ((new infestation-card%))
          (send resolve test-board)
          (send game-over?)
          check-false)))
  (test-case
    "thwarting causes -4 damage and removes the enemy"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (damage-ship damage)
               (check-equal? damage -4)
               (super damage-ship damage)))))
    (parameterize ([ui (test-ui '([(pick-action thwart battle warp) thwart]))])
      (~> ((new infestation-card%))
          (send resolve test-board)
          (send game-over?)
          check-true))))


;; (define-card/encounter solar-flair-card%
;;   #:name "Solar flair"
;;   #:text "Roll. If you lose, place top 3 cards in the lost stack and take -2 \
;;           damage to the ship"
;;   #:stats (#:defense 7 #:life 1)
;;
;;   (special-rolls
;;     [miss? -2])
;;   
;;   (when miss
;;     (~> (board)
;;         (feedback 3 (send draw-card))
;;         (feedback 3 (==* (send _ discard _ #:stack 'lost) _))
;;   #:discard graveyard)

(define solar-flair-card%
  (class encounter-card%
    (super-new)
    
    (define/override (describe)
      ((ui) `(describe-card
               #:name "Solar flair"
               #:text "Roll. If you lose, place top 3 cards in the losts stack and take -2 damage to the ship"
               #:defense 7 #:life -1)))

    (define/override (make-enemy)
      (new
        (class enemy%
          (super-new)
          (define/override (special-rolls roll)
            (cond
              [(< roll 7) -2]
              [else #f])))
        [defense 7] [attack 7] [life -1]))

    (define/override (miss board enemy)
      (~> (board)
          (feedback 3 (==* (send draw-card) _))
          (feedback (while (~> count (> 1)))
                    (group 2 (send _ discard _ #:stack 'lost) _))
          (values enemy)))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))))

(module+ solar-flair-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (test-case
    "missing causes -2 damage and discards top three cards"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (damage-ship damage)
               (check-equal? damage -2)
               (super damage-ship damage)))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 5]
                                  (enemy-evaded)
                                  (enemy-defeated)))])
      (~> ((new solar-flair-card%))
          (send resolve test-board)
          (-< (~> (send game-over?)
                  check-false)
              (~> (get-field stacks _)
                  (hash-ref 'lost)
                  length
                  (check-equal? 3)))))))

;TODO: Keep track of total-misses
;; (define-card/encounter time-eater-card%
;;   #:name "Time eater"
;;   #:text "Every miss: place the top card of your deck in the lost stack and \
;;           age +10 years. If this happens 3 times, place Time Eater back into \
;;           the deck and shuffle"
;;   #:stats (#:defense 10 #:life 3)
;;
;;   (special-rolls
;;     [miss? 0])
;;
;;   (when miss
;;     (~> (board)
;;         (send draw-card)
;;         (send _ discard _ #:stack 'lost)
;;         (send age-pilot 10)
;;         (if (equal? total-misses 2)
;;             (send shuffle-into-deck #:stack this)
;;             (values _ enemy))))
;;   
;;   #:discard graveyard)

(define time-eater-card%
  (class encounter-card%
    (super-new)
    
    (define/override (describe)
      ((ui) `(describe-card
               #:name "Time eater"
               #:text "Every miss: place the top card of your deck in the lost stack and age +10 years. If this happens 3 times, place Time Eater back into the deck and shuffle."
               #:defense 10 #:life 3)))

    (define/override (make-enemy)
      (new
        (class enemy%
          (super-new)
          (define/override (special-rolls roll)
            (cond
              [(< roll 10) 0]
              [else #f])))
        [defense 10] [attack 9] [life 3]))

    (define/override (miss board enemy)
      (~> (board)
          (send draw-card)
          (send _ discard _ #:stack 'lost)
          (send age-pilot 10)
          (if (~> (gen (get-field misses enemy))
                  (equal? 3))
              (~> (send shuffle-into-deck #:stack this)
                  (values (send enemy damage 3)))
              (values enemy))))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))))

(module+ time-eater-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (define test-board (new test-board%))
  (test-case
    "missing sends the top deck card to the lost stack and ages +10 years"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (age-pilot years)
               (check-equal? years 10)
               this))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 1]
                                  (enemy-evaded)
                                  [(pick-action battle warp) warp]))])
      (~> ((new time-eater-card%))
          (send resolve test-board)
          (-< (~> (send game-over?)
                  check-false)
              (~> (get-field stacks _)
                  (hash-ref 'lost)
                  length
                  (check-equal? 2))
              (~> (get-field aged _)
                  check-true)))))
  (test-case
    "missing three times shuffles time-eater back into the deck"
    (define test-board
      (new (class test-board%
             (super-new)
             (define/override (age-pilot years)
               (check-equal? years 10)
               this))))
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 1]
                                  (enemy-evaded)
                                  [(pick-action battle warp) battle]
                                  [(roll d20) 1]
                                  (enemy-evaded)
                                  [(pick-action battle warp) battle]
                                  [(roll d20) 1]
                                  (enemy-evaded)
                                  (enemy-damaged #:damage 3 #:life 0)
                                  (enemy-defeated)))])
      (~> ((new time-eater-card%))
          (send resolve test-board)
          (~> (get-field stacks _)
              (-< (~> (hash-ref 'deck)
                      length
                      (check-equal? 1))))))))
                  ;; (~> (hash-ref 'graveyard '())
                  ;;     length
                  ;;     (check-equal? 0))))))))

;; (define-card/encounter warp-scourge-card%
;;   #:name "Warp scourge"
;;   #:text "You must roll two consecutive hits to deal -1 damage"
;;   #:stats (#:defense 12 #:attack 6 #:life 2)
;;
;;   (special-rolls
;;     [(and hit? (> consecutive-hits 0)) 1])
;;
;;   #:discard graveyard)

(define warp-scourge-card%
  (class encounter-card%
    (super-new)
    
    (define/override (describe)
      ((ui) `(describe-card
               #:name "Warp scourge"
               #:text "You must roll two consecutive hits to deal -1 damage"
               #:defense 12 #:attack 6 #:life 2)))

    (define/override (make-enemy)
      (new
        (class enemy%
          (super-new)
          (inherit-field consecutive-hits)
          (define/override (special-rolls roll)
            (cond
              [(and (>= roll 12)
                    (>= consecutive-hits 2)) 1]
              [(>= roll 12) 0]
              [(> roll 6) -1]
              [else #f])))
        [defense 12] [attack 6] [life 2]))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))))

(module+ warp-scourge-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (define test-board (new test-board%))
  (test-case
    "hitting only once does no damage"
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-evaded)
                                  [(pick-action battle warp) warp]))])
      (~> ((new warp-scourge-card%))
          (send resolve test-board)
          (-< (~> (send game-over?)
                  check-false)))))
  (test-case
    "hitting twice does damage"
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-evaded)
                                  [(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-damaged #:damage 1 #:life 1)
                                  [(pick-action battle warp) warp]))])
      (~> ((new warp-scourge-card%))
          (send resolve test-board)
          (-< (~> (send game-over?)
                  check-false)))))
  (test-case
    "alternate hits and misses do no damage"
    (parameterize ([ui (test-ui '([(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-evaded)
                                  [(pick-action battle warp) battle]
                                  [(roll d20) 11]
                                  (enemy-evaded)
                                  [(pick-action battle warp) battle]
                                  [(roll d20) 20]
                                  (enemy-evaded)
                                  [(pick-action battle warp) battle]
                                  [(roll d20) 11]
                                  (enemy-evaded)
                                  [(pick-action battle warp) warp]))])
      (~> ((new warp-scourge-card%))
          (send resolve test-board)
          (-< (~> (send game-over?)
                  check-false))))))

;TODO: Contest action (think of a way to configure the rounds)

;; (define-card/encounter enemy-fleet-card%
;;   #:name "Enemy fleet"
;;   #:text "Win 2 of 3 rolls to escape. Otherwise, take -3 damage, and place \
;;           enemy fleet in lost stack."
;;   #:stats (#:defense 12 #:life 2)
;;
;;   (disable-action battle)
;;   (enable-action contest)
;;
;;   (when defeat
;;     (~> (board)
;;         (send damage-ship -3)
;;         (send discard this #:stack 'lost)))
;;
;;   #:discard graveyard)

(define enemy-fleet-card%
  (class encounter-card%
    (super-new)
    
    (define/override (describe)
      ((ui) `(describe-card
               #:name "Enemy fleet"
               #:text "Win 2 of 3 rolls to escape. Otherwise, take -3 damage, and place enemy fleet in the lost stack"
               #:defense 12)))

    (define/override (make-enemy)
      (new enemy% [defense 12]
                  [attack 11]
                  [life -1]))

    (define/override (available-actions)
      (~>> ((super available-actions))
           (remove 'battle)
           (cons 'my-contest)))

    (inherit contest)

    (define/public (my-contest board enemy)
      (contest 2 board enemy))

    (define/augment (defeat board)
      (~> (board)
          (send damage-ship -3)
          (send discard this #:stack 'lost)))

    (define/override (discard board)
      (send board discard this #:stack 'graveyard))))

(module+ enemy-fleet-card%-tests
  (require (submod ".." examples))
  (require (submod "../ui.rkt" examples))
  (define test-board
    (new (class test-board%
           (super-new)
           (define/override (damage-ship damage)
             (check-equal? damage -3)
             this))))
  (test-case
    "losing two rolls loses, damges the ship and sends card to lost stack"
    (parameterize ([ui (test-ui '([(pick-action my-contest warp) my-contest]
                                  (contest #:goal 2)
                                  [(roll d20) 1]
                                  (contest-failure)
                                  [(roll d20) 1]
                                  (contest-failure)
                                  (player-defeated)))])
      (~> ((new enemy-fleet-card%))
          (send resolve test-board)
          (-< (~> (send game-over?)
                  check-false)
              (~> (get-field stacks _)
                  (-< (~> (hash-ref 'lost)
                          length
                          (check-equal? 1))
                      (~> (hash-ref 'graveyard '())
                          length
                          (check-equal? 0)))))))))

(define all-cards
  (list
    (new plasma-cruiser-card%)
    (new marauder-card%)
    (new star-strider-card%)
    (new nebula-raider-card%)
    (new infestation-card%)
    (new solar-flair-card%)
    (new time-eater-card%)
    (new warp-scourge-card%)
    (new enemy-fleet-card%)))


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
                   [destroyed #f]
                   [stacks (hash)])
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
       (define/public (draw-card) (values this (new dummy-card% [name 'test])))
       (define/public (discard c #:stack [destination #f])
         (clone #:stacks (hash-update stacks destination (curry cons c) list)))
       (define/public (shuffle-into-deck #:stack c)
         (clone #:stacks (hash-update stacks 'deck (curry cons c) list)))
       (define (clone #:aged [aged aged]
                      #:damaged [damaged damaged]
                      #:dead [dead dead]
                      #:destroyed [destroyed destroyed]
                      #:stacks [stacks stacks])
         (new test-board% [aged aged]
                          [damaged damaged]
                          [dead dead]
                          [destroyed destroyed]
                          [stacks stacks])))))

(module+ test
  (require (submod ".." encounter-card%-tests))
  (require (submod ".." plasma-cruiser-card%-tests))
  (require (submod ".." marauder-card%-tests))
  (require (submod ".." star-strider-card%-tests))
  (require (submod ".." nebula-raider-card%-tests))
  (require (submod ".." infestation-card%-tests))
  (require (submod ".." solar-flair-card%-tests))
  (require (submod ".." time-eater-card%-tests))
  (require (submod ".." warp-scourge-card%-tests))
  (require (submod ".." enemy-fleet-card%-tests)))
