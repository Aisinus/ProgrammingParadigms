#lang slideshow

(define (render-bit color)
  (cond
    [(= color 0) (rectangle 10 10)]
    [(= color 1) (filled-rectangle 10 10)]
    )
  )


(define (render-bits list)
  (cond
    [(empty? list) (filled-rectangle 0 0)]
    [else (hc-append (render-bit (first list))
                     (render-bits (rest list)) )]
   )
  )

(filled-rectangle 0 0)
(render-bit 1)
(render-bit 0)
(render-bits '(1 0 0 1))