#!r6rs

(module list-processing
    (provide (all-defined-out))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; not in racket's implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define atom?
    (lambda (x)
      (and (not (pair? x))
           (not (null? x)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic routines for list manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;; is the list 'l' a list of atoms?
  (define lat?
    (lambda (l)
      (cond ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))
  
;;; is the atom 'a' in the list of atoms 'lat'?
  (define member?
    (lambda (a lat)
      (cond ((null? lat) #f)
            ((eq? a (car lat)) #t)
            (else (member? a (cdr lat))))))
  
;;; generalized version of member?
  (define member*
    (lambda (a l)
      (cond ((null? l) #f)
            ((atom? (car l))
             (or (eq? (car l) a)
                 (member* a (cdr l))))
            (else (or (member* a (car l))
                      (member* a (cdr l)))))))

;;; remove the atom 'a' from the list of atoms 'lat' 
  (define rember
    (lambda (a lat)
      (cond ((null? lat) '())
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
                        (rember a (cdr lat)))))))

;;; remove all occurences of a in lat
  (define multirember
    (lambda (a lat)
      (cond ((null? lat) '())
            ((eq? (car lat) a) (multirember a (cdr lat)))
            (else (cons (car lat)
                        (multirember a (cdr lat)))))))

;;; generalized version of rember 
  (define rember*
    (lambda (a l)
      (cond ((null? l) '())
            ((atom? (car l))
             (cond ((eq? a (car l))(rember* a (cdr l)))
                   (else (cons (car l)
                               (rember* a (cdr l))))))
            (else (cons (rember* a (car l))
                        (rember* a (cdr l)))))))

;;; l is either the empty list, or a list of non empty lists
;;; firsts builds up a new list with the first symbolic expr.
;;; of each of the sublists
  (define firsts
    (lambda (l)
      (cond ((null? l) '())
            (else (cons (car (car l))
                        (firsts (cdr l)))))))

;;; insert the atom new to the right of old in lat
  (define insertR
    (lambda (new old lat)
      (cond ((null? lat) '())
            (else (cond ((eq? (car lat) old) (cons old
                                                   (cons new
                                                         (cdr lat))))
                        (else (cons (car lat)
                                    (insertR new old (cdr lat)))))))))

;;; multi version of insertR
  (define multiinsertR
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((eq? (car lat) old)(cons old
                                      (cons new
                                            (multiinsertR new old (cdr lat)))))
            (else (cons (car lat)
                        (multiinsertR new old (cdr lat)))))))

;;; generalized version of insertR
  (define insertR*
    (lambda (new old l)
      (cond ((null? l) '())
            ((atom? (car l))
             (cond ((eq? (car l) old)
                    (cons old
                          (cons new
                                (insertR* new old (cdr l)))))
                   (else (cons (car l)
                               (insertR* new old (cdr l))))))
            (else (cons (insertR* new old (car l))
                        (insertR* new old (cdr l)))))))

;;; insert the atom new to the left of old in lat
  (define insertL
    (lambda (new old lat)
      (cond ((null? lat) '())
            (else (cond ((eq? (car lat) old) (cons new lat))
                        (else (cons (car lat)
                                    (insertL new old (cdr lat)))))))))


;;; multi version of insertL
  (define multiinsertL
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((eq? (car lat) old)(cons new
                                      (cons old
                                            (multiinsertL new old (cdr lat)))))
            (else (cons (car lat)
                        (multiinsertL new old (cdr lat)))))))

;;; generalized version of insertL
  (define insertL*
    (lambda (new old l)
      (cond ((null? l) '())
            ((atom? (car l))
             (cond ((eq? (car l) old)
                    (cons new
                          (cons old
                                (insertL* new old (cdr l)))))
                   (else (cons (car l)
                               (insertL* new old (cdr l))))))
            (else (cons (insertL* new old (car l))
                        (insertL* new old (cdr l)))))))

;;; substitute the atom old with new in the lat
  (define subst
    (lambda (new old lat)
      (cond ((null? lat) '())
            (else (cond ((eq? (car lat) old)
                         (cons new (cdr lat)))
                        (else (cons (car lat)
                                    (subst new old (cdr lat)))))))))
;;; multi version of subst
  (define multisubst
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((eq? (car lat) old) (cons new
                                       (multisubst new old (cdr lat))))
            (else (cons (car lat)
                        (multisubst new old (cdr lat)))))))

;;; generalized version of subst
  (define subst*
    (lambda (new old l)
      (cond ((null? l) '())
            ((atom? (car l))
             (cond ((eq? (car l) old)
                    (cons new
                          (subst* new old (cdr l))))
                   (else (cons (car l)
                               (subst* new old (cdr l))))))
            (else (cons (subst* new old (car l))
                        (subst* new old (cdr l)))))))

;;; length of a lat
  (define length
    (lambda (lat)
      (cond ((null? lat) 0)
            (else (add1 (length (cdr lat)))))))

;;; pick the nth elem of lat (condition: there is at least n elems)
  (define pick
    (lambda (n lat)
      (cond ((zero? (sub1 n)) (car lat))
            (else (pick (sub1 n) (cdr lat))))))

;;; remove the nth elem in lat
  (define rempick
    (lambda (n lat)
      (cond ((one? n) (cdr lat))
            (else (cons (car lat)
                        (rempick (sub1 n)
                                 (cdr lat)))))))

;;; removes number atoms from a lat
  (define no-nums
    (lambda (lat)
      (cond ((null? lat)(quote ()))
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat)
                        (no-nums (cdr lat)))))))

;;; same as above, but collects nums in a tuple
  (define all-nums
    (lambda (lat)
      (cond ((null? lat)(quote ()))
            ((number? (car lat)) (cons (car lat)
                                       (all-nums (cdr lat))))
            (else (all-nums (cdr lat))))))
  
;;; how many times does the atom a occur in lat?
  (define occur
    (lambda (a lat)
      (cond ((null? lat) 0)
            ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
            (else (occur a (cdr lat))))))

;;; generalized version of occur
  (define occur*
    (lambda (a l)
      (cond ((null? l) 0)
            ((atom? (car l))
             (cond ((eq? (car l) a)
                    (add1 (occur* a (cdr l))))
                   (else (occur* a (cdr l)))))
            (else (plus (occur* a (car l))
                        (occur* a (cdr l)))))))

;;; return leftmost atom in a list of s-expressions (can't be '())
  (define leftmost
    (lambda (l)
      (cond ((atom? (car l)) (car l))
            (else (leftmost (car l))))))

;;; equality test
  (define equal?
    (lambda (s1 s2)
      (cond ((and (atom? s1) (atom? s2))
             (eqan? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else (eqlist? s1 s2)))))

;;; compares 2 list, returns #t if lists are identical
  (define eqlist?
    (lambda (l1 l2)
      (cond ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            ((and (atom? (car l1)) (atom? (car l2)))
             (and (eqan? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2))))
            ((or (atom? (car l1)) (atom? (car l2))) #f) 
            ((and (eqlist? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2))))
            (else (or (list? (car l1)) (list? (car l2))) #f))))

;;; end of module
  )
)

