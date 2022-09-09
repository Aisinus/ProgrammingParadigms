#lang slideshow


(define (count-ones lst)
  (define (helper lst current)
    (cond
      [(empty? lst) current]
      [(= (first lst) 1) (helper (rest lst)
                                 (+ current 1))]
      [else (helper (rest lst)
                    current)]
      )
    )
  (helper lst 0)
  )


(define (number lst)
  (define (helper lst current)
          (cond
               [(empty? lst) current]
               [(= (first lst) 1) (helper (rest lst)
                                          (+ current (expt 2 (- (length lst) 1))))]
               [else (helper (rest lst) current)]
               )
          )
  (helper lst 0)
)

(define (trailing-zeros lst)
  (define (helper number current)
    (cond
      [(= (modulo number 2) 0) (helper (/ number 2) (+ current 1))]
      [else current]
      )
    )
  (helper (number lst) 0)
  )


(trailing-zeros '(1 1 1 0 0 0))