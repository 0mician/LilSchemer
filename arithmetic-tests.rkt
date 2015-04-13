#lang racket/base

(provide arithmetic-tests)

(require "arithmetic.scm")
(require rackunit)

(define-test-suite arithmetic-tests
  "tests for arithmetic.rkt"
  ;;; one?
  (check-equal? (one? 1) #t "1 is equal to 1")
  (check-equal? (one? -3) #f "1 is not equal to -3")
  (check-equal? (one? 0.01) #f "1 is not equal to 0.01")
  ;;; plus
  (check-equal? (plus 4 5) 9 "4+5 is 9")
  (check-equal? (plus 0 0) 0 "0+0 is 0")
  ;;; minus
  (check-equal? (minus 5 4) 1 "5-4 is 1")
  (check-equal? (minus 0 0) 0 "0-0 is 0")
  ;;; multiply
  (check-equal? (multiply 10 20) 200 "10*20 is 200")
  (check-equal? (multiply 0 10) 0 "0*10 is 0")
  ;;; addtup
  (check-equal? (addtup '(1 2 3 4)) 10 "1+2+3+4 is 10")
  (check-equal? (addtup '(0)) 0 "addtup 0 is 0")
  ;;; tup+
  (check-equal? (tup+ '(1 2 3 4) '(1 2 3 4)) '(2 4 6 8)
                "(1 2 3 4) + (1 2 3 4) is (2 4 6 8)")
  ;;; tup2+
  (check-equal? (tup2+ '(1 3) '(1 4 5)) '(2 7 5)
                "(1 3) + (1 4 5) is (2 7 5)")
  (check-equal? (tup2+ '() '(1 3)) '(1 3)
                "() + (1 3) is (1 3)")
  ;;; gt?
  (check-equal? (gt? 3 4) #f "3<4 is false")
  (check-equal? (gt? 10 0) #t "10>0 is true")
  (check-equal? (gt? 0 0) #f "0>0 is false")
  ;;; lt?
  (check-equal? (lt? 3 4) #t "3<4 is true")
  (check-equal? (lt? 10 0) #f "10<0 is false")
  (check-equal? (lt? 0 0) #f "0<0 is false")
  ;;; eql?
  (check-equal? (eql? 4 4) #t "4 == 4 is true")
  (check-equal? (eql? 10 0) #f "10 == 0 is false")
  (check-equal? (eql? 0 0) #t "0 == 0 is true")
  ;;; intdiv
  (check-equal? (intdiv 13 4) 3 "13 // 4 is 3")
  (check-equal? (intdiv 4 4) 1 "4 // 4 is 1")
  (check-equal? (intdiv 0 2) 0 "0 // 2 is 0")
  ;;; eqan?
  (check-equal? (eqan? 4 4) #t "4 == 4")
  (check-equal? (eqan? 'trl 'trl) #t "trl == trl")
  (check-equal? (eqan? 'mass 0) #f "mass is not equal to 0")
  (check-equal? (eqan? '0 0) #t "'0 == 0")
  ;;; numbered?
  (check-equal? (numbered? '(4 + (3 * 3))) #t
                "(4 + (3 * 3)) is a valid arithmetic construction")
  (check-equal? (numbered? '(2 + 3)) #t
                "(2 + 3) is a valid arithmetic construction")
  (check-equal? (numbered? '(3 + + (3 -))) #f
                "(3 + + (3 -)) is not a valid arithmetic construction"))
