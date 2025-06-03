#lang racket

(provide (struct-out board))

(struct board (deck left-card right-card lost-stack graveyard))
