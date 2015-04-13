#lang racket/base

(provide atom-tests)

(require "atom.scm")
(require rackunit)

(define-test-suite atom-tests
  "tests for atom.scm"
  (check-equal? (atom? 'turkey) #t "character atom is accepted")
  (check-equal? (atom? '1492) #t "number atom is accepted")
  (check-equal? (atom? '()) #f "empty list is not an atom")
  (check-equal? (atom? '(a b c)) #f "non empty list is not an atom"))

