#lang racket

(require racket/generator)


(provide ui)


(define ui
  (generator (msg)
    (displayln "Solus lost")
    (let loop ([msg msg])
      (match msg
        ['(quit) (displayln "Game over")]
        ['(roll d20) (displayln "Press enter to roll a d20")
                     (read-string 1)
                     (define d20 (add1 (random 20)))
                     (displayln (~a "You rolled: " d20))
                     (loop (yield d20))]
        ['(choose-card) (displayln "Choose a card: (l)eft or (r)ight")
                        (define choice
                          (let loop ()
                            (define choice (string-trim (read-line)))
                            (if (member choice '("l" "r")) choice (loop))))
                        (loop (yield choice))]
        ['(game-over) (displayln "Game over")]
        [msg (displayln msg)
             (loop (yield))]))))

