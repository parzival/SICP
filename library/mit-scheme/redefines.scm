; (Re) define a few procedures to be more compatible to Racket file
; Note that this file cannot be loaded more than once in the same session. 

(if (environment-bound? (the-environment) 'redefined-mit-functions)
  (begin 
  (display "File already loaded ... this redefinition file can only be loaded once.")
  (abort)
 )
)

(load-option 'format)
(define format-mit format)
(unbind-variable (the-environment) 'format)
(define (format s . args) (apply format-mit (cons false (cons s args)))) 

(define old-random random)
(unbind-variable (the-environment) 'random)

(define (random . x)
  (if (null? x)
      (old-random 1.0)
      (old-random (car x))
      )
  )
(define redefined-mit-functions true)


