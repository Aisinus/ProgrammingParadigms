#lang slideshow
; Exercise 1.1 `(+ x 1) '1 'x

(define (variable? expr)
  (cond
  [(not (symbol? expr)) false]
    )
    true
  )

 (variable? (+ 1 2)) 
