#lang slideshow

; ## a
(define (alternating-sum lst)
  (cond
    [(empty? lst) 0]
    [else (- (first lst) (alternating-sum (rest lst)))]
    )
  )

(alternating-sum (list 6 2 4 1 3))
(alternating-sum (list 1 2 3 4 5))
; ## b
; alternating-sum -> a
; first -> f
; (a '(1 2 3 4 5))
; (- (f '(1 2 3 4 5)) (a '(2 3 4 5)))
; (- (f '(1 2 3 4 5)) (- (f '(2 3 4 5)) (a '(3 4 5))))
; (- (f '(1 2 3 4 5)) (- (f '(2 3 4 5)) (- (f'(3 4 5)) (a '(4 5)))))
; (- (f '(1 2 3 4 5)) (- (f '(2 3 4 5)) (- (f'(3 4 5)) (- (f '(4 5)) (a '(5))))))
; (- (f '(1 2 3 4 5)) (- (f '(2 3 4 5)) (- (f'(3 4 5)) (- (f '(4 5)) (- 5 0))))
; (- (f '(1 2 3 4 5)) (- (f '(2 3 4 5)) (- (f'(3 4 5)) (- 4 5)))
; (- (f '(1 2 3 4 5)) (- (f '(2 3 4 5)) (- 3 -1))
; (- (f '(1 2 3 4 5)) (- 2 4))
; (- 1 -2)
; 3

; ## c
; In this case, tail recursion will help to avoid allocationg a new stack frame for a function because the calling function will simply return the value that it gets from the called function.
