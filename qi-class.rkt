#lang racket

(require qi)
(require rackunit)
(require (for-syntax syntax/parse))

(provide
  (for-space qi
    send make-object get-field new))

(define-qi-foreign-syntaxes send make-object get-field)

(define-qi-syntax-parser new
  #:literals (_)
  [(new _ (~or [varv _] [var rhs]) ...)
   #'(λ (cls varv ...) (new cls [varv varv] ... [var rhs] ...))]
  [(new cls (~or [varv _] [var rhs]) ...)
   #'(λ (varv ...) (new cls [varv varv] ... [var rhs] ...))]
  [new #'(λ (cls) (new cls))])

(module+ new-tests
  (test-case
    "value can be used as the class"
    (define foo% (class object% (super-new)))
    (~> (foo%)
        new
        (is-a? foo%)
        check-true))
  (test-case
    "value can be used as the class explicitly"
    (define foo% (class object% (super-new)))
    (~> (foo%)
        (new _)
        (is-a? foo%)
        check-true))
  (test-case
    "value can be used as the class while passing arguments"
    (define foo% (class object% (init-field bar) (super-new)))
    (~> (foo%)
        (new _ [bar 3])
        (-< (~> (is-a? foo%)
                check-true)
            (~> (get-field bar _)
                (check-equal? 3)))))
  (test-case
    "value can be used as an argument"
    (define foo% (class object% (init-field bar) (super-new)))
    (~> (7)
        (new foo% [bar _])
        (get-field bar _)
        (check-equal? 7)))
  (test-case
    "multiple values can be used as arguments"
    (define foo% (class object% (init-field bar baz) (super-new)))
    (~> (7 3)
        (new foo% [bar _] [baz _])
        (-< (~> (get-field bar _)
                (check-equal? 7))
            (~> (get-field baz _)
                (check-equal? 3)))))
  (test-case
    "provided and missing init variables can be mixed"
    (define foo% (class object% (init-field bar baz) (super-new)))
    (~> (3)
        (new foo% [bar 7] [baz _])
        (-< (~> (get-field bar _)
                (check-equal? 7))
            (~> (get-field baz _)
                (check-equal? 3)))))
  (test-case
    "one value can be used as the class and the rest as arguments"
    (define foo% (class object% (init-field bar baz) (super-new)))
    (~> (foo% 7 3)
        (new _ [bar _] [baz _])
        (-< (~> (is-a? foo%)
                check-true)
            (~> (get-field bar _)
                (check-equal? 7))
            (~> (get-field baz _)
                (check-equal? 3))))))

(module+ test
  (require (submod ".." new-tests)))
