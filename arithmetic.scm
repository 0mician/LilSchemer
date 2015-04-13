(module arithmetic racket
  (require "atom.scm")
  (provide (all-defined-out))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithmetic routines - educational purpose only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test if n is 1
  (define one?
    (lambda (n)
      (eq? n 1)))

;;; sum of 2 natural numbers
  (define plus
    (lambda (n1 n2)
      (cond ((zero? n2) n1)
            (else (add1 (plus n1 (sub1 n2)))))))

;;; difference
  (define minus
    (lambda (n1 n2)
      (cond ((zero? n2) n1)
            (else (sub1 (minus n1 (sub1 n2)))))))

;;; sum of elements of a tuple of natural numbers
  (define addtup
    (lambda (tup)
      (cond ((null? tup) 0)
            (else (plus (car tup)
                        (addtup (cdr tup)))))))

;;; multiplication of natural numbers
  (define multiply
    (lambda (n1 n2)
      (cond ((zero? n2) 0)
            (else (plus n1
                        (multiply n1 (sub1 n2)))))))

;;; sum of tuples (same length, natural numbers)
  (define tup+
    (lambda (tup1 tup2)
      (cond ((and (null? tup1) (null? tup2)) (quote ()))
            (else (cons (plus (car tup1) (car tup2))
                        (tup+ (cdr tup1) (cdr tup2)))))))

;;; generalized version (different length
  (define tup2+
    (lambda (tup1 tup2)
      (cond ((null? tup1) tup2)
            ((null? tup2) tup1)
            (else (cons (plus (car tup1) (car tup2))
                        (tup2+ (cdr tup1) (cdr tup2)))))))

;;; greater than?
  (define gt?
    (lambda (n1 n2)
      (cond ((zero? n1) #f)
            ((zero? n2) #t)
            (else (gt? (sub1 n1)
                       (sub1 n2))))))

;;; lesser than?
  (define lt?
    (lambda (n1 n2)
      (cond ((zero? n2) #f)
            ((zero? n1) #t)
            (else (lt? (sub1 n1)
                       (sub1 n2))))))

;;; equality test
  (define eql?
    (lambda (n1 n2)
      (cond ((and (zero? n1) (zero? n2)) #t)
            ((or (zero? n1) (zero? n2)) #f)
            (else (eql? (sub1 n1)
                        (sub1 n2))))))

;;; integer division
  (define intdiv
    (lambda (n1 n2)
      (cond ((< n1 n2) 0)
            (else (add1 (intdiv (minus n1 n2) n2))))))

;;; generalized equality between two atoms
  (define eqan?
    (lambda (a1 a2)
      (cond ((and (number? a1) (number? a2))(eql? a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))

;;; determines if an expression is an Arithmetic expression
  (define numbered?
    (lambda (aexp)
      (cond ((atom? aexp)(number? aexp))
            ((eq? (car (cdr aexp)) '+)
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) '-)
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) '*)
             (and (numbered? (car aexp))
                  (numbered? (car (cdr (cdr aexp)))))))))
  
;;; end of module
  )

