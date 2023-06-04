; Exercises 3.5.1

(display "Including library files...")
(include "library/streambasic.scm")
(display-line "done.")

; Ex 3.50
; Generalized stream-map

(define (stream-map proc . argstreams)
;  (if (<??> (car argstreams))
      the-empty-stream
;      (<??> 
;       (apply proc (map <??> argstreams))
;       (apply stream-map (cons proc (map <??> argstreams)))
;       )
;      )
 )

; Testing
(newline)
(display-line "Testing stream-map implementation")
(define s1 (stream-enumerate-interval 0 10))
(define s2 (stream-enumerate-interval 1 11))
(define s3 (stream-enumerate-interval 2 12))

(display-line "Actual results of stream-map: ")
(display-stream (stream-map + s1 s2))

(display-line "Comparing stream-map results on multiple streams:")

; stream-map Test 1
(display "Test 1, sum of two streams:")
(define expected1 (stream-filter (lambda(x) (not (even? x))) (stream-enumerate-interval 1 22))) 
(stream-test = (stream-map + s1 s2) expected1)

; stream-map Test 2
(display "Test 2, using stream twice as the argument:")
(define expected2 (list-to-stream '( 2 8 18 32 50 72 98 128 162 200 242 ) ))

(stream-test =
             (stream-map (lambda (x1 x2) (* x1 (+ x2 x2))) s2 s2)
             expected2
             )

; stream-map Test 3
(display "Test 3, using three streams:")
(define alphalist '( a b c d e f g h i j k l m n o p q r s t u v w x y z ) )
(define expected3 (list-to-stream '( (a b c) (b c d) (c d e) (d e f) (e f g) (f g h) (g h i) (h i j) (i j k) (j k l) (k l m) )))
(stream-test equal?
             (stream-map (lambda (x1 x2 x3) (map (lambda (l) (list-ref alphalist l)) (list x1 x2 x3))) s1 s2 s3)
             expected3
             )

; Ex 3.51
; Examining delayed execution (compare when delay is not memoized)

(newline)
(display-line "Demonstrating results of delayed evaluation")

(define (show x)
  (display-line x)
  x
  )

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

; Ex 3.52
;
(newline)
(display-line "Examining a sequence of expressions (memoization check)")

;Consider this sequence
; What is the value of sum at each step?
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum
  ) 

(define seq
  (stream-map accum (stream-enumerate-interval 1 20))
  )

(define y (stream-filter even? seq))

(define z (stream-filter 
           (lambda (x) (= (remainder x 5) 0))
           seq
           )
  )

(stream-ref y 7)
(display-stream z)
