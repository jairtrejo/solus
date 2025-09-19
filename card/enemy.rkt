#lang racket

(provide enemy%)

(require qi)
(require rackunit)
(require "../qi-class.rkt")
(require "../ui.rkt")


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


(define (ui-enemy-damaged damage remaining-life)
  ((ui) `(enemy-damaged #:damage ,damage #:life ,remaining-life)))


(define (ui-enemy-evaded)
  ((ui) '(enemy-evaded)))


(define (ui-roll-result result)
  ((ui) `(roll-result ,result)))


(define enemy
  (interface () handle-roll
                defeated?))


(define enemy%
  (class object%
    (super-new)

    (init-field defense attack life)

    (init-field [misses 0]
                [hits 0]
                [consecutive-hits 0]
                [consecutive-misses 0])

    (define/pubment (damage-in roll)
      (define special-damage
        (inner #f damage-in roll))
      (~> (roll)
          (switch
            [(gen special-damage) (gen special-damage)]
            [(>= defense) roll->enemy-damage]
            [(<= attack) 0]
            [else 1])))

    (define/pubment (damage-out roll)
      (define special-damage
        (inner #f damage-out roll))
      (~> (roll)
          (switch
            [(gen special-damage) (gen special-damage)]
            [(>= defense) 0]
            [(<= attack) roll->ship-damage]
            [else -1])))

    (define/public (damage-self amount)
      (~> (amount)
          (- life _)
          (effect (ui-enemy-damaged amount _))
          (send this clone #:life _)))

    (define/public (hit roll board)
      (ui-roll-result 'hit)
      (~>> (roll)
           (send this damage-in)
           (send this damage-self)
           (send _ clone #:hits (add1 hits)
                         #:consecutive-hits (add1 consecutive-hits)
                         #:consecutive-misses 0)
           (values board)))

    (define/public (miss roll board)
      (ui-roll-result 'miss)
      (~> (roll)
          (-< (~>> (send this damage-out)
                   (send board damage-ship))
              (~> (gen (send this clone
                             #:misses (add1 misses)
                             #:consecutive-misses (add1 consecutive-misses)
                             #:consecutive-hits 0))))))

    (define/public (tie board)
      (ui-roll-result 'tie)
      (on (board this)
        (== (send damage-ship -1)
            (~> (send damage-self 1)
                (send clone #:consecutive-hits 0
                            #:consecutive-misses 0)))))

    (define/public (handle-roll roll board)
      (~> (roll)
        (switch
          [(>= defense) (send this hit _ board)]
          [(>= attack) (send this tie _ board)]
          [(< attack) (send this miss _ board)])))

    (define/public (defeated?)
      (not (positive? life)))

    (define/public (clone #:defense [defense defense]
                        #:attack [attack attack]
                        #:life [life life]
                        #:hits [hits hits]
                        #:misses [misses misses]
                        #:consecutive-hits [consecutive-hits consecutive-hits]
                        #:consecutive-misses [consecutive-misses consecutive-misses])
      (new this% [defense defense]
                 [attack attack]
                 [life life]
                 [hits hits]
                 [misses misses]
                 [consecutive-hits consecutive-hits]
                 [consecutive-misses consecutive-misses]))))


(module+ enemy%-tests
  (require (submod "../ui.rkt" examples))
  (require (submod "card.rkt" examples))
  (define test-enemy (new enemy% [defense 14] [attack 5] [life 3]))
  (define test-board (new test-board%))
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
        (send damage-self 2)
        (get-field life _)
        (check-equal? 1))))
  (test-case
    "it is defeated after being damaged enough"
    (parameterize ([ui (test-ui '((enemy-damaged #:damage 4 #:life -1)))])
      (~> (test-enemy)
        (send damage-self 4)
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
      (check-equal? 0)))
  (test-case
    "damages enemy if special roll"
    (define test-enemy
      (new
        (class enemy%
          (super-new)
          (define/augment (damage-in roll)
            (cond
              [(> roll 10) 999]
              [else #f])))
        [defense 14] [attack 5] [life 3]))
    (~> (test-enemy)
      (send damage-in 20)
      (check-equal? 999)))
  (test-case
    "damages ship if special roll"
    (define test-enemy
      (new
        (class enemy%
          (super-new)
          (define/augment (damage-out roll)
            (cond
              [(< roll 10) -999]
              [else #f])))
        [defense 14] [attack 5] [life 3]))
    (~> (test-enemy)
      (send damage-out 1)
      (check-equal? -999)))
  (test-case
    "damages normally if the roll is not special"
    (define test-enemy
      (new
        (class enemy%
          (super-new)
          (define/augment (damage-in roll)
            (cond
              [(> roll 18) 999]
              [else #f])))
        [defense 14] [attack 5] [life 3]))
    (~> (test-enemy)
      (send damage-in 15)
      (check-equal? 2)))
  (test-case
    "counts hits and misses"
    (parameterize ([ui (test-ui '((roll-result hit)
                                  (enemy-damaged #:damage 4 #:life -1)
                                  (roll-result miss)
                                  (roll-result hit)
                                  (enemy-damaged #:damage 4 #:life -5)))])
      (~>> (test-board test-enemy)
        (~> X (send _ handle-roll 20 _))
        (~> X (send _ handle-roll 1 _))
        (~> X (send _ handle-roll 20 _))
        2>
        (-< (~> (get-field hits _)
                (check-equal? 2))
            (~> (get-field misses _)
                (check-equal? 1))))))
  (test-case
    "counts consecutive hits"
    (parameterize ([ui (test-ui '((roll-result hit)
                                  (enemy-damaged #:damage 4 #:life -1)
                                  (roll-result hit) 
                                  (enemy-damaged #:damage 4 #:life -5)
                                  (roll-result miss) 
                                  (roll-result hit) 
                                  (enemy-damaged #:damage 4 #:life -9)))])
      (~>> (test-board test-enemy)
           (~> X (send _ handle-roll 20 _))
           (effect (~> 2> (get-field consecutive-hits _) (check-equal? 1)))
           (~> X (send _ handle-roll 20 _))
           (effect (~> 2> (get-field consecutive-hits _) (check-equal? 2)))
           (~> X (send _ handle-roll 1 _))
           (effect (~> 2> (get-field consecutive-hits _) (check-equal? 0)))
           (~> X (send _ handle-roll 20 _))
           2> (get-field consecutive-hits _)
           (check-equal? 1))))
  (test-case
    "counts consecutive misses"
    (parameterize ([ui (test-ui '((roll-result miss)
                                  (roll-result miss)
                                  (roll-result hit)
                                  (enemy-damaged #:damage 4 #:life -1)
                                  (roll-result miss)))])
      (~> (test-board test-enemy)
        (~> X (send _ handle-roll 1 _))
        (effect (~> 2> (get-field consecutive-misses _) (check-equal? 1)))
        (~> X (send _ handle-roll 1 _))
        (effect (~> 2> (get-field consecutive-misses _) (check-equal? 2)))
        (~> X (send _ handle-roll 20 _))
        (effect (~> 2> (get-field consecutive-misses _) (check-equal? 0)))
        (~> X (send _ handle-roll 1 _))
        2> (get-field consecutive-misses _)
        (check-equal? 1)))))

(module+ test
  (require (submod ".." enemy%-tests)))
