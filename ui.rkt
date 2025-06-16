#lang racket

(require racket/generator)

(require rackunit)

(provide terminal-ui)


(define terminal-ui
  (generator (init-msg)
    (displayln "Solus lost")
    (let loop ([msg init-msg])
      (displayln "")
      (match msg
        ['(quit) (displayln "Game over")]
        ['(roll d20) (displayln "Press enter to roll a d20")
                     (read-string 1)
                     (define d20 (add1 (random 20)))
                     (displayln (~a "You rolled: " d20))
                     (loop (yield d20))]
        ['(choose-card) (displayln "Choose a card: (n)ew or (o)ld")
                        (define choice
                          (let loop ()
                            (define choice (string-trim (read-line)))
                            (if (member choice '("n" "o")) choice (loop))))
                        (loop (yield choice))]
        ['(game-over) (displayln "Game over")]
        [msg (displayln msg)
             (loop (yield))]))))


(module+ examples
  (provide test-ui)
  (define (test-ui msg-descs)
    (generator (init-msg)
      (let loop ([msg-descs msg-descs]
                 [actual-msg init-msg])
        (check-false (empty? msg-descs) (~a "Received extra message " actual-msg))
        (match (first msg-descs)
          [(list (list expected-msg ...) res)
           (check-equal? actual-msg expected-msg)
           (loop (rest msg-descs) (yield res))]
          [(var expected-msg)
           (check-equal? actual-msg expected-msg)
           (loop (rest msg-descs) (yield))])))))


(module+ test-ui-tests
  (require qi)
  (require (submod ".." examples))
  (test-case
    "allows messages in the the message description list"
    (~> ((test-ui '((some-message))))
        (apply '((some-message)))
        live?
        check-false))
  (test-case
    "returns specified result from the message description list"
    (~> ((test-ui '([(some-message) some-result])))
        (apply '((some-message)))
        (check-equal? 'some-result)))
  (test-case
    "supports multiple messages"
    (~> ((test-ui '((initial-message) [(some-message) some-result])))
        (-< (apply '((initial-message)))
            (apply '((some-message))))
        (check-equal? 'some-result)))
  (test-case
    "does not allow unknown messages"
    (check-exn
      exn:test:check?
      (λ ()
        (~> ((test-ui '((initial-message) (some-message))))
            (-< (apply '((initial-message)))
                (apply '((unknown-message))))))))
  (test-case
    "does not allow extra messages"
    (check-exn
      exn:test:check?
      (λ ()
        (~> ((test-ui '((initial-message))))
            (-< (apply '((initial-message)))
                (apply '((extra-message)))))))))


(module+ test
  (require (submod ".." test-ui-tests)))
