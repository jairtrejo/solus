#lang racket

(provide card card?)

(require qi)
(require rackunit)
(require "../ui.rkt")


(define card
  (interface ()
             describe
             resolve))


(module+ examples
  (provide dummy-card% test-board%)

  (define dummy-card%
    (class* object% (card)
      (super-new)

      (init-field name)

      (define/public (describe)
        ((ui) `(describe-card ,name)))

      (define/public (resolve board)
        ((ui) `(resolving-card ,name))
        board)))

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



(define-flow card? (is-a? card))


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


(module+ test
  (require (submod ".." card?-tests)))
