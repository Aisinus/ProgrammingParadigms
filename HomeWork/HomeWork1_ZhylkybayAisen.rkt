#lang slideshow

;Zhylkybay Aisen SD-02
; My functions

(define (operator? arg)
    (cond
      [(empty? arg) #f]
      [else (ormap (lambda (x) (equal? arg x)) '(+ - * / ^ = < >))]
      )
  )

(define (term_n expr n)
  (define (get_term expr n k)
    (cond
      [(equal? n k) (first expr)]
      [else (get_term (rest expr) n (+ 1 k))]
      )
    )
  (cond
    [(not (number? n)) (error "Expected a n summand from expression, but got" n)]
    [(> 1 n) (error "Expected n>=1 summand from expression, but got" n)]
    [(< (length expr) (+ n 1)) (error "Expected n < terms in expression but got" n)]
    [else (get_term expr n 0)]
    )
  )

(define (expression_type? expr type)
  (define (helper expr isoperation ok)
    (cond
      [(empty? expr) ok]
      [(and isoperation (operator? (first expr))) (helper (rest expr) #f #t)]
      [(and (not isoperation) (or (variable? (first expr)) (number? (first expr)))) (helper (rest expr) #f #t)]
      [(and (not isoperation) (pair? (first expr)) (> (length (first expr)) 2)) (helper (first expr) #t #f)]
      [else #f]
      )
    )
  (cond
    [(symbol? expr) #f]
    [(< (length expr) 3) #f]
    [(equal? (first expr) type) (helper (rest expr) #f #f)]
    [else #f]
   )
  )

(define (to_list arg)
  (cond
    [(list? arg) arg]
    [else (list arg)]
    )
  )


; Exercise 1.1

(define (variable? expr)
  (cond
  [(not (symbol? expr)) #f]
  [else (not (operator? expr))]
    )
  )


(define (sum? expr)
  (expression_type? expr `+)
  )



(define (summand-1 expr)
  (cond
    [(not (sum? expr)) (error "Expected a sum expression of the form `(+ <expr> <expr>), but got: " expr)]
    [else (term_n expr 1)]
   )
  )

(define (summand-2 expr)
  (cond
    [(not (sum? expr)) (error "Expected a sum expression of the form `(+ <expr> <expr>), but got: " expr)]
    [else (term_n expr 2)]
   )
  )


(define (product? expr)
  (expression_type? expr `*)
  )


(define (multiplier-1 expr)
  (cond
    [(not (product? expr)) (error "Expected a product expression of the form `(* <expr> <expr>), but got: " expr)]
    [else (term_n expr 1)]
   )
  )

(define (multiplier-2 expr)
  (cond
    [(not (product? expr)) (error "Expected a product expression of the form `(* <expr> <expr>), but got: " expr)]
    [else (term_n expr 2)]
   )
  )


; Exercise 1.2

(define (derivative exrp arg)
  (define (helper expr arg)
    (cond
      [(list? expr)(cond
                     [(sum? expr) (append (list) (map (lambda (x) (helper x arg)) expr) )]
                     [(product? expr) (append (list `+) (map (lambda (x) (append (list `* (helper x arg)) (remw x (rest expr))) ) (rest expr)))]
                     [else (error "Invalid input of expression: " expr)]
                     )]
      [(variable? expr) (cond
                          [(equal? expr arg) `1]
                          [else `0]
                          )]
      [(number? expr) `0]
      [else expr ]
      )
    ) 
  (cond
    [(operator? arg) (error "Expected a symbol, but got:" arg)]
    [else (helper exrp arg)]
    )
  )

; Exercise 1.3

(define (simplify expr)
  (define (helper expr)
    (cond
      [(not (list? expr)) expr]
      [(sum? expr) (cond
                    [(andmap number? (rest expr)) (apply + (rest expr))]
                    [(and (not (empty? (filter number? expr))) (equal? (apply + (filter number? (rest expr))) 0)) (cond
                                                                                                                    [( > (length (filter-not number? expr)) 2 ) (append `() (map (lambda (x) (helper x)) (filter-not number?  expr)))]
                                                                                                                    [(operator? (first (rest (filter-not number? expr)))) (append `() (list (first (rest (filter-not number? expr)))))]
                                                                                                                    [else (first (rest (filter-not number? expr)))]
                                                                                                                    )]
                    [else (append `(+)
                                  (map (lambda (var) (helper var) ) (filter-not number? (rest expr)))
                                  (remw `0 (list (apply + (filter number? (rest expr))))))] 
                    )] 
      [(product? expr) (cond
                         [(ormap (lambda (x) (equal? x `0)) (rest expr)) 0]
                         [(andmap number? (rest expr)) (apply * (rest expr))]
                         [(and (not (empty? (filter number? expr))) (equal? (apply * (filter number? (rest expr))) 1)) (cond
                                                                                                                         [(> (length (filter-not number? expr)) 2) (append `() (map (lambda (x) (helper x)) (filter-not number?  expr)))]
                                                                                                                         [(operator? (first (rest (filter-not number? expr)))) (append `() (list (first (rest (filter-not number? expr)))))]
                                                                                                                         [else (first (rest (filter-not number? expr)))]
                                                                                                                         )]
                         [else (append `(*)
                                   (map (lambda (var) (helper var) ) (filter-not number? (rest expr)))
                                   (remw `1 (list (apply * (filter number? (rest expr))))))]
                         )]
      [else (list expr)]
      )
    
    )
  
  
  (cond
    [(equal? (helper expr) (helper (helper expr)) ) (helper expr)]
    [else (simplify (helper expr))]
    )
)
 

; Exercise 1.5


(define (to-infix expr)
  (define (helper expr)
    (cond
      [(empty? expr) expr]
      [(or (variable? (first expr)) (number? (first expr))) (to_list(first expr))]
      [(sum? expr) (list (append (helper (to_list (summand-1 expr))) `() `(+) (helper (to_list (summand-2 expr))) `() ))]
      [(product? expr) (list (append (helper (to_list (multiplier-1 expr))) `() `(*) (helper (to_list (multiplier-2 expr))) `()))]
      [else (error "invalid input data" expr)]
      )
    )
  (cond
    [(empty? expr) (error "Executed to-infix in the form `(<operator> <expr> <expr>),but got" expr)]
    )
  (first (helper expr))
  )


; Exercise 1.6
; Did only for derivative
(define (derivative-1.6 exrp arg)
  (define (helper expr arg)
    (cond
      [(list? expr)(cond
                     [(sum? expr) (append (list) (map (lambda (x) (helper x arg)) expr) )]
                     [(product? expr) (append (list `+) (map (lambda (x) (append (list `* (helper x arg)) (remw x (rest expr))) ) (rest expr)))]
                     [(equal? (first expr) `log) (append (list `* (list `/ 1 (first (rest expr))) (map (lambda (x) (helper x arg)) (rest expr))))]
                     [(equal? (first expr) `cos) (append (list `* (list `-sin (first (rest expr))) (map(lambda (x) (helper x arg)) (rest expr))))]
                     [(equal? (first expr) `sin) (append (list `* (list `cos (first (rest expr))) (map(lambda (x) (helper x arg)) (rest expr))))]
                     [(equal? (first expr) `^) (cond
                                                 [(and (equal? (first (rest expr)) arg) (= (length (to_list (first (rest (rest expr))))) 1)) (append (list `*) (to_list (first (rest (rest expr)))) (list (list `^ (first (rest expr)) (list `-  (first (rest (rest expr))) 1))))]
                                                 [(ormap (lambda (x) (equal? x arg)) (to_list (first (rest expr)))) (append (list `*) (list `^ `e (list `* (first (rest (rest expr))) (list `* `log (first (rest expr))))) (map (lambda (x) (helper x arg))(list `* (first (rest (rest expr))) (list `* `log (first (rest expr))))))]
                                                 [(ormap (lambda (x) (equal? x arg)) (to_list (first (rest (rest expr))))) (append (list `*) (list (list `log (first (rest expr)))) (list expr) (map (lambda (x) (helper x arg)) (to_list (first (rest (rest expr))))))]
                                                 [else (list 0)]
                                                 )]
                     [else (error "Invalid input of expression: " expr)]
                     )]
      [(variable? expr) (cond
                          [(equal? expr arg) `1]
                          [else `0]
                          )]
      [(number? expr) `0]
      [else expr ]
      )
    ) 
  (cond
    [(operator? arg) (error "Expected a symbol, but got:" arg)]
    [else (helper exrp arg)]
    )
  )


; Exercise 1.7
; I already wrote the functions which support polyvariadic

(derivative '(+ 1 x y (* x y z)) 'x)
(simplify '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0))))

; Exercise 1.8

(define (variables-of expr)
  (append (filter-not list? expr) (filter-not list? (filter list? expr)))
  )

(variables-of '(+ 1 x y (* 1 2)))