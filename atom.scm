#lang racket/base

(provide atom?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; not in racket's implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define atom?
    (lambda (x)
      (and (not (pair? x))
           (not (null? x)))))
