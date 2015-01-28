; Ex. 1.34
; Passing functions as arguments

;(require trace)

(define (f g)
  ((g 2)
  )

(f sqr)

(f (lambda (z) (* z (+ z 1))))

(f display)



; What happens when we do this?
 (f f)
