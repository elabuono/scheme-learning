;;; Author: Arthur Nunes-Harwitt
;;; Author: Erica LaBuono exl2748

;;; data definitions

;; An arithmetic expression (ArithExp) is one of the following.
;; a number n
;; a variable x
;; a sum with parts e1 and e2, 
;;   where e1 and e2 are arithmetic expressions
;; a product with parts e1 and e2,
;;   where e1 and e2 are arithmetic expressions

;; functions for associated with each part: predicate, constructor, selectors.

;; number is a Scheme number

;; variable is a Scheme symbol

; variable?: Any -> Bool
(define variable? symbol?)

; (eq? (variable? 'x) #t)
; (eq? (variable? 3) #f)

; variable=?: VarExp VarExp -> Bool
(define variable=? eq?)

; (variable=? 'x 'x)
; (not (variable=? 'x 'y))

;; a sum is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol +

; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))

; (eq? (sum? '(+ 2 3)) #t)
; (eq? (sum? '3) #f)


; make-sum: ArithExp ArithExp -> SumExp
(define (make-sum e1 e2)
  (cond ((and (number? e1) (= e1 0)) e2)
        ((and (number? e2) (= e2 0)) e1)
        (else (list '+ e1 e2))))

; (equal? (make-sum 2 3) '(+ 2 3))

;; a product is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol *

; product?: Any -> Bool
(define (product? a) (and (pair? a) (eq? (car a) '*)))

; (eq? (product? '(* 2 3)) #t)
; (eq? (product? '3) #f)

; make-product: ArithExp ArithExp -> ProductExp
(define (make-product e1 e2)
  (cond ((and (number? e1) (= e1 0)) 0)
        ((and (number? e2) (= e2 0)) 0)
        ((and (number? e1) (= e1 1)) e2)
        ((and (number? e2) (= e2 1)) e1)
        (else (list '* e1 e2))))

; (equal? (make-product 2 3) '(* 2 3))

; PART 3
; make-expt: ArithExp, Number -> ExptExp
(define (make-expt e n)
  (cond ((= n 0) 1)
        ((= n 1) e)
    (else (list '^ e n))))
; (equal? (make-expt 2 3) '(^ 2 3))

; expt?: Any -> Bool
(define (expt? a) (and (pair? a) (eq? (car a) '^)))

;; sums and products will use the same selectors

; arg1: SumExp or ProdExp -> ArithExp
(define (arg1 e) (car (cdr e)))

; (= (arg1 (make-sum 2 3)) 2)
; (= (arg1 (make-product 2 3)) 2)

; arg2: SumExp or ProdExp -> ArithExp
(define (arg2 e) (car (cdr (cdr e))))

; (= (arg2 (make-sum 2 3)) 3)
; (= (arg2 (make-product 2 3)) 3)

;;; derivative code

;deriv: ArithExp VarExp -> ArithExp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (variable=? exp var) 1 0))
        ((sum? exp) 
         (make-sum (deriv (arg1 exp) var)
                   (deriv (arg2 exp) var)))
        ((product? exp)
         (make-sum (make-product (arg1 exp) (deriv (arg2 exp) var))
                   (make-product (arg2 exp) (deriv (arg1 exp) var))))
        ((expt? exp)
         (make-product (make-product (arg2 exp) (deriv (arg1 exp) var)) ; product of a product and expt
                   (make-expt (arg1 exp) (- (arg2 exp) 1))))
        (else (error 'deriv "Unexpected Input, not an ArithExp"))))

; test cases
;(= (deriv 1 'x) 0)
;(= (deriv 'y 'x) 0)
;(= (deriv 'x 'x) 1)
;(equal? (deriv (make-expt 'x 3) 'x) '(* 3 (^ x 2))) ; returns 3x^2