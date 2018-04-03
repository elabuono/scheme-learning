; Assign1B: creating merge and merge sort
; Author: Erica LaBuono exl2748

; merge : List(Num), List(Num) -> List(Num)
; merge takes two ordered lists of nums and combines them in order
(define (merge ordered1 ordered2)
  (if (null? ordered1) ; return other list when one list is empty
      ordered2
  (if (null? ordered2)
      ordered1
  (if (< (car ordered1) (car ordered2)) ; if the 1st list 1st val < 2nd list 1st val
      (cons (car ordered1) (merge (cdr ordered1) ordered2)) ; append val 1 to list 2, merge again
      (cons (car ordered2) (merge (cdr ordered2) ordered1)))))) ; otherwise, append the val 2 to list 1 and merge
; test cases:
;(merge '(1 4 6) '(2 3 7)) ; outputs (1 2 3 4 6 7)
;(merge '(1 1 2 2)  '(1 100)) ; outputs (1 1 1 2 2 100)

; split-odd and split-even : List(Num) -> List(Num)
; splitting allows for an every-other split of lists for merge sort
; split-odd
(define (split-odd lst)
  (if (null? lst) ; if the lst is empty, return empty list
      '()
  (if (null? (cdr lst)) ; if the lst is size 1, return that!! as a new list
      (list (car lst))
  (cons (car lst) (split-odd (cddr lst)))))) ; construct lst by skipping over next element
     
; split-even
(define (split-even lst)
  (if (null? lst)  ; return empty lst if list is empty
      '()
  (if (null? (cdr lst)) ; if the lst is size 1, ignore first element
      '()
   (cons (cadr lst) (split-even (cddr lst)))))) ; construct lst by skipping over first element

; merge-sort : List(Num) -> List(Num)
; mergesort uses splitting and remerging on a list of numbers to sort
(define (mergesort nums)
  (if (null? nums) ; return the nums if it's empty
      nums
  (if (null? (cdr nums)) ; return the nums if it is size 1, 1 element remaining
      nums
  (merge (mergesort (split-odd nums))
         (mergesort (split-even nums))))))

; test cases:
;(mergesort '(2 1 5 7 9 6))
;(mergesort '(100))
;(mergesort '(7 8 9))