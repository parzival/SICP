; Streambasic: This file has the basic routines for working with streams,
; and also defines delay/force as special forms if they are needed.
; Be sure to uncomment the desired definition of (delay) below. 

; Convenience function to print output followed by a newline
(define (display-line ln)
  (display ln)
  (newline)
  )

; Many Scheme implementations have (force) and (delay) 
; already defined, although they may or may not use memoization.

; ** NOTE **
; If any version of delay in this file is used, force should be defined explicitly,
; along with cons-stream and other stream procedures in this file.

; Memoized form of delay (cf. text's *memo-proc*)

(define-syntax delay
  (syntax-rules ()
    ((_ x) (let ((already-run? false)
                 (result false)
                   )
               (lambda ()
                 (if (not already-run?)
                     (begin
                       (set! result x)
                       (set! already-run? true)
                       result
                       )
                     result
                     )
                 )
               )
             )
    )
  )


; Alternate versions of delay for testing

; Delay will not evaluate arguments, but is not memoized
;(define-syntax delay
;  (syntax-rules ()
;    ((_ x) (lambda () x))
;    )
;   )

; Delay is just a lambda expression [not properly delaying]
;(define (delay x)
;  (lambda () x)
;  )

; Forcing a delay, for all versions
(define (force delayed-object) (delayed-object))


; Special form for cons-stream
; To prevent arguments from being evaluated
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b))
             )
    )
  )


; Basic Stream operations
(define (stream-car stream) (car stream)) 
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))
      )
  )

(define (stream-map proc s)
  (if (stream-null? s) 
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s))
                   )
      )
  ) 

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s)) 
        (stream-for-each proc (stream-cdr s))
        )
      )
  )
  
; Additional stream functions

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low 
       (stream-enumerate-interval (+ low 1) high)
       )
      )
  )

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream
         )
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter pred (stream-cdr stream))
          )
         ) 
        (else (stream-filter pred (stream-cdr stream)))
        )
  )

; Compact version, displays with spaces only 
(define (display-stream-compact s)
  (stream-for-each (lambda (s) (display s) (display " ")) s)
  )

; Default version, each element on one line
(define (display-stream s)
  (stream-for-each display-line s)
  )

; This produces a stream applying a predicate to both streams.
; Appends a value of 'false' if the streams differ in length, 
; or 'true' if they are the same length (including zero-length).
(define (stream-compare pred s1 s2)
  (cond ((stream-null? s1)
         (if (stream-null? s2)
             (cons-stream true the-empty-stream)
             (cons-stream false the-empty-stream)
             )
         )
        ((stream-null? s2) (cons-stream false the-empty-stream)) ; Already checked if s1 is done.
        (else (cons-stream (pred (stream-car s1) (stream-car s2))
                           (stream-compare pred (stream-cdr s1) (stream-cdr s2))
                           )
              )
        )
  )

; Tests if all elements of a stream are true; exits on first false element
(define (stream-and s)
  (if (stream-null? s)
      true
      (if (stream-car s)
          (stream-and (stream-cdr s))
          false
          )
      )
  )

; Shortcut for testing that stream-compare passed for every element
(define (stream-test pred s1 s2)
  (stream-and (stream-compare pred s1 s2))
  )

(define (list-to-stream li)
  (if (null? li)
      the-empty-stream
      (cons-stream (car li) (list-to-stream (cdr li)))
      )
  )
