#lang racket

(require qi)
(require (for-syntax syntax/parse))

(provide (struct-out board))
(provide (for-space qi struct-copy update-deck))

(struct board (deck lost-stack graveyard))


(define-qi-syntax-parser struct-copy
  #:literals (_)
  [(struct-copy cls _ (~or [varv _] [var rhs]) ...)
   #'(λ (obj varv ...) (struct-copy cls obj [varv varv] ... [var rhs] ...))]
  [(struct-copy cls obj (~or [varv _] [var rhs]) ...)
   #'(λ (varv ...) (struct-copy cls obj [varv varv] ... [var rhs] ...))])


(define-qi-syntax-rule (update-deck flo)
  (~> (-< _ (~> board-deck flo))
      (group 2 (struct-copy board _ [deck _]) _)))
