
; Generalized stream-map (allows multiple stream arguments)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream 
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams)))
       )
      )
  )


; New functions on streams

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1) 
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               )
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2))
                  ) 
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2)))
                  )
                 (else
                  (cons-stream s1car (merge (stream-cdr s1)
                                            (stream-cdr s2)
                                            )
                               )
                  )
                 )
           )
         )
        )
  )

; Functions suitable for infinite streams
; Note : Don't use "display-stream" on an infinite stream 

; Creates a limited stream
(define (limit-stream n s)
  (if (= n 0)
    the-empty-stream
    (cons-stream (stream-car s) 
                 (limit-stream (- n 1) (stream-cdr s))
                 )
    )
  )

(define (get-n-of-stream n s)
  (if (or (= n 0) (stream-null? s))
      '()
      (cons (stream-car s) (get-n-of-stream (- n 1) (stream-cdr s)))
      )
  )

(define (display-n-of-stream n s)  
  (display (get-n-of-stream n s))
  (newline)
  )

; This version will put each item in the stream on a separate line.
(define (display-line-n-of-stream n str)
  (for-each display-line (get-n-of-stream n str))
  )

; Special streams

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))