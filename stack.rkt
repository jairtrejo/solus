#lang racket

(require qi)
(require rackunit)
(require syntax/parse/define)

(require "qi-class.rkt")

(provide stack
         stack?
         exn:empty-stack?
         exn:stack-overflow?
         list->stack
         stack->list)

(define stack
  (interface ()
             put
             take
             empty?
             shuffle
             shuffle-in))


(define (stack? s)
  (is-a? s stack))


(define stack%
  (class* object% (stack)
    (super-new)

    (abstract put take empty?)

    (define (stack-shuffle)
      (~> (this)
          stack->list
          shuffle
          list->stack))

    (define (shuffle-in s)
      (~> (this s)
          (>< stack->list)
          append
          list->stack
          (send shuffle)))

    (public [stack-shuffle shuffle] shuffle-in)))


(module+ stack%-tests
  (require (for-syntax syntax/parse))
  (define-syntax-parser with-random-seed
    [(with-random-seed seed body ...) 
     #'(parameterize ([current-pseudo-random-generator
                       (make-pseudo-random-generator)])
         (random-seed seed)
         body ...)])
  (test-case
    "a non-empty stack can be shuffled"
    (with-random-seed 42
      (~> ((list->stack '(a b c d)))
          (send shuffle)
          stack->list
          (check-equal? '(b d c a)))))
  (test-case
    "an empty stack can be shuffled"
    (~> ((list->stack '()))
        (send shuffle)
        (send empty?)
        check-true))
  (test-case
    "a fixed-capacity stack can be shuffled"
    (with-random-seed 42
      (~> ((list->stack '(a b c d) 4))
          (send shuffle)
          (-< (~> (is-a? limited-stack%)
                  check-true)
              (~> stack->list
                  (check-equal? '(b d c a)))))))
  (test-case
    "a non-empty stack can be shuffled into another"
    (with-random-seed 42
      (~> ((list->stack '(1 2 3 4))
           (list->stack '(5 6)))
          (send _ shuffle-in _)
          stack->list
          (check-equal? '(2 5 3 1 6 4)))))
  (test-case
    "an empty stack can be shuffled into a non-empty stack"
    (with-random-seed 42
      (~> ((list->stack '(1 2 3 4)) (list->stack '()))
          (send _ shuffle-in _)
          stack->list
          (check-equal? '(2 4 3 1)))))
  (test-case
    "a non-empty stack can be shuffled into an empty stack"
    (with-random-seed 42
      (~> ((list->stack '()) (list->stack '(1 2 3 4)))
          (send _ shuffle-in _)
          stack->list
          (check-equal? '(2 4 3 1))))))


(define nonempty-stack%
  (class* stack% (stack)
    (init-field cards)
    
    (super-new)

    (define/override (put c)
      (~>> (cards)
          (cons c)
          (new nonempty-stack% [cards _])))

    (define/override (take)
      (~> (cards)
          (-< (~> rest
                  list->stack)
              first)))

    (define/override (empty?) #f)))

(module+ nonempty-stack%-tests
  (test-case
    "take removes a card from the top of the stack"
    (~> ((new nonempty-stack% [cards '(a b c)]))
        (send take)
        2>
        (check-equal? 'a)))
  (test-case
    "put adds a card at the top of the stack"
    (~> ((new nonempty-stack% [cards '(a b c)]))
        (send put 'x)
        (send take)
        2>
        (check-equal? 'x)))
  (test-case
    "taking the last card yields an empty stack"
    (~> ((new nonempty-stack% [cards '(a b c)]))
        (feedback 3 (~> (send take) 1>))
        (send empty?)
        check-true)))

(struct exn:empty-stack exn:fail ())
(define (raise-empty-stack-error)
  (raise (exn:empty-stack "Stack is empty"
                          (current-continuation-marks))))

(define empty-stack%
  (class* stack% (stack)
    (super-new)

    (define/override (put c)
      (list->stack (list c)))

    (define/override (take)
      (raise-empty-stack-error))

    (define/override (empty?) #t)))

(module+ empty-stack%-tests
  (test-case
    "stack starts empty"
    (~> ((new empty-stack%))
        (send empty?)
        check-true))
  (test-case
    "cards cannot be taken"
    (check-exn
      exn:empty-stack?
      (λ ()
        (~> ((new empty-stack%))
            (send take)))))
  (test-case
    "put yields a nonempty stack with that card at the top"
    (~> ((new empty-stack%))
        (send put 'x)
        (-< (~> (is-a? nonempty-stack%)
                check-true)
            (~> (send take)
                2>
                (check-equal? 'x))))))

(struct exn:stack-overflow exn:fail ())
(define (raise-stack-overflow-error)
  (raise (exn:stack-overflow "Stack overflow"
                          (current-continuation-marks))))

(define limited-stack%
  (class* stack% (stack)
    (super-new)

    (init-field capacity)
    (init [cards '()])

    (define inner-stack
      (~> (cards) list->stack))

    (define remaining-capacity (- capacity (length cards)))

    (cond [(negative? remaining-capacity) (raise-stack-overflow-error)])

    (define/override (put c)
      (~> (this)
          (unless (send full?) (~> (gen inner-stack)
                                   (send put c)
                                   stack->list
                                   (list->stack capacity)))
          (rectify (raise-stack-overflow-error))))

    (define/override (take)
      (~> (inner-stack)
          (send take)
          (== (~> stack->list
                  (list->stack capacity))
              _)))

    (define/override (empty?) (send inner-stack empty?))

    (define/override (shuffle)
      (~> (inner-stack)
          (send shuffle)
          stack->list
          (list->stack capacity)))

    (define/public (full?)
      (~> (remaining-capacity) zero?))))

(module+ limited-stack%-tests
  (test-case
    "can add cards when below capacity"
    (~> ((new limited-stack% [capacity 3] [cards '(a b)]))
        (send put 'x)
        (send take)
        2>
        (check-equal? 'x)))
  (test-case
    "can't add cards above capacity"
    (check-exn
      exn:stack-overflow?
      (λ ()
        (~> ((new limited-stack% [capacity 1] [cards '(a)]))
            (send put 'x)))))
  (test-case
    "taking cards when full restores capacity"
    (~> ((new limited-stack% [capacity 3] [cards '(a b c)]))
        (-< (~> (send full?) check-true)
            (~> (send take)
                1> (send full?) check-false)))))

(define-flow (stack->list s)
  (~> (feedback (while (~> 1> (send empty?) NOT))
                (==* (send take) _))
      (block 1)
      X
      collect))

(module+ stack->list-tests
  (test-case
    "an empty stack produces an empty list"
    (~> ((new empty-stack%))
        stack->list
        (check-equal? '())))

  (test-case
    "a non-empty stack produces its contents"
    (~> ((new nonempty-stack% [cards '(a b c)]))
        stack->list
        (check-equal? '(a b c)))))

(define (list->stack l [capacity #f])
  (~> (capacity l)
      (switch [1> (gen (new limited-stack% [capacity capacity] [cards l]))]
              [(~> 2> empty?) (gen (new empty-stack%))]
              [else (~> 2> (new nonempty-stack% [cards _]))])))

(module+ list->stack-tests
  (test-case
    "an empty list results in an empty stack"
    (~> ('())
        list->stack
        (send empty?)
        check-true))
  (test-case
    "a non-empty list results in a non-empty stack"
    (~> ('(a b c))
        list->stack
        (-< (~> (send empty?)
                check-false)
            (~> (send take)
                2>
                (check-equal? 'a)))))
  (test-case
    "passing a capacity results in a limited stack"
    (~> ('(a b c) 3)
        list->stack
        (is-a? limited-stack%)
        check-true)))

(module+ test
  (require (submod ".." stack%-tests))
  (require (submod ".." nonempty-stack%-tests))
  (require (submod ".." empty-stack%-tests))
  (require (submod ".." limited-stack%-tests))
  (require (submod ".." stack->list-tests))
  (require (submod ".." list->stack-tests)))
