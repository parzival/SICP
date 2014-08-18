; SECTION 2.2.3

(define (announce phrase) 
  (newline)
  (display phrase)
  (newline)
  )

; defined in the text

(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial 
      (op (car sequence)
          (accumulate op initial (cdr sequence))
          )
      )
  )


(define (filter-ex predicate sequence) 
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter-ex predicate (cdr sequence))
               )
         ) 
        (else (filter-ex predicate (cdr sequence)))
        )
  )

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))
      )
  )

(define (enumerate-tree tree) 
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))
                      )
              )
        )
  )


; Ex 2.33
; Fill in the missing expressions using accumulate 

; using -acc due to later requirement for map in 2.37
(define (map-acc p sequence)
  (accumulate (lambda (x y) <??>) null sequence)
  ) 

(define (append-acc seq1 seq2)
  (accumulate cons <??> <??>)
  ) 

(define (length-acc sequence)
  (accumulate <??> 0 sequence)
  )


; Testing
(announce "Testing map, append, length")
(define 1-to-4 (list 1 2 3 4))
(equal? (list 1 4 9 16) (map-acc (lambda (x) (* x x)) (list 1 2 3 4)))
(define list1 (list -10 2.5 -11.6 17))
(equal? (map abs list1) (map-acc abs list1))
(equal? (append list1 1-to-4) (append-acc list1 1-to-4))
(equal? (length 1-to-4) (length-acc 1-to-4))

; Ex 2.34
; Fill in the template for polynomial evaluation using Horner's Rule

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                <??>
                )
              0
              coefficient-sequence
              )
  )

; Testing
(announce "Testing Horner's Rule evaluation")
(equal? 79 (<?horner's?> 2 (list 1 3 0 5 0 1)))

; Ex 2.35
; Redefine 'count-leaves' using accumlation.

(define (count-leaves t)
  (accumulate <??> <??> (map <??> <??>))
  )

; Testing
(announce "Testing count-leaves")
(define x (cons (list 1 2) (list 3 4))) 
(length x)
(count-leaves x)
(count-leaves (list x (list x)))


; Ex 2.36
; Complete the definition of accumulate-n

(define (accumulate-n op init seqs) 
  (if (null? (car seqs))
      null
      (cons (accumulate op init <??>)
            (accumulate-n op init <??>)
            )
      )
  )

; Testing
(announce "Testing accumulate-n")
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)

; Ex 2.37
; Complete the definitions of matrix operations using the representation below.

; A vector v is a sequence of numbers.
; A matrix m is represented as a sequence of vectors in row order.

(define (dot-product v w)
  (accumulate + 0 (map * v w))
  )

(define (matrix-*-vector m v)
  (map <??> m)
  ) 

(define (transpose mat)
  (accumulate-n <??> <??> mat)
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)
    )
  )

; Testing
(announce "Testing matrix operations")
(define A (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list -1  2  3  1))
(define w (list  2 -2  3 -3))

(dot-product v w)
(dot-product w v)
(matrix-*-vector A v)
(transpose A)
(matrix-*-matrix A (transpose A))
(matrix-*-matrix A (transpose (list v)))


; Ex 2.38
; Give a property that op should have to yield the same result for fold-left and fold-right.

(define (fold-left op initial sequence) 
  (define (iter result rest)
    (if (null? rest) 
        result
        (iter (op result (car rest)) (cdr rest))
        )
    )
  (iter initial sequence)
  )

(define fold-right accumulate)

(announce "Verifying fold-right")
(fold-right / 1 (list 1 2 3)) 
(fold-left / 1 (list 1 2 3)) 
(fold-right list null (list 1 2 3)) 
(fold-left list null (list 1 2 3))


; Ex 2.39
; Complete the definitions of reverse using fold-left and fold-right.

(define (reverse-r sequence)
  (fold-right (lambda (x y) <??>) null sequence)
  )

(define (reverse-l sequence)
  (fold-left (lambda (x y) <??>) null sequence)
  )

; Testing
(announce "Testing revese using left and right")
(reverse-r 1-to-4)
(reverse-l 1-to-4)  ; Should yield the same result
(reverse-l list1)

; Ex 2.40
; Define (unique-pairs) to provide a sequence of pairs.  

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond 
    ((> (sqr test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
    )
  )
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (flatmap proc seq) (accumulate append null (map proc seq)))

(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair) 
  (prime? (+ (car pair) (cadr pair)))
  )

; Use (unique-pairs) to simplify (prime-sum-pairs)

(define (prime-sum-pairs n)
  <??>
  )

; Testing
(announce "Testing unique-pairs and prime-sum-pairs")
; test n = 4  set is (1,2),(1,3),(1,4),(2,3),(2,4),(3,4)
; test n = 6  set is (2+1=3,3+2=5,4+1=5,4+3=7,5+2=7,6+1=7,6+5=11)

; Ex 2.41
; Write a procedure to generate the set of distinct ordered triples < n that sum to a given value s

(announce "Testing triples")
;             n   s
(sum-triples  6   9   ); 3 triples
(sum-triples  9  57   ); none 
(sum-triples  3   6   ); 1 triple (1,2,3)
(sum-triples 10  14   ); 9 triples
(sum-triples  2   2   ); none - no triples exist for n <= 2
(sum-triples -5   1   ); none - n need not be positive.
(sum-triples 20  -1   ); none - s need not be positive.
(sum-triples  5 -10   ); none - i,j,k must be positive
; Ex 2.42
; Complete the necessary definitions in order to solve the queens puzzle

(define (queens board-size) 
  (define (queen-cols k)
    (if (= k 0) 
        (list empty-board) 
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens) 
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens)
                                  ) 
                                (enumerate-interval 1 board-size)
                                )
                           )
                         (queen-cols (- k 1))
                         )
                )
        )
    )
  (queen-cols board-size)
  )


; Testing 

; (print-solution) is required to display a solution.  
; (queens) returns a sequence of solutions.
; Only when k=0 is a list containing a null solution returned; the other
; cases with no solution yield an empty list

(announce "Testing queens")
(for-each print-solution (queens 0))
(for-each print-solution (queens 1))
(queens 2)
(queens 3)
(for-each print-solution (queens 4)) ; First case with an interesting solution
(for-each print-solution (queens 5))
(for-each print-solution (queens 6))
(for-each print-solution (queens 8))
(length (queens 8))
(length (queens 9))
;(length (queens 12))

; Number of solutions for a given board size
;0                       0
;1                       1               
;2                       0             
;3                       0             
;4                       2             
;5                      10             
;6                       4             
;7                      40            
;8                      92            
;9                     352            
;10                    724            
;11                  2,680            
;12                 14,200            
;13                 73,712            
;14                365,596            
;15              2,279,184            
;16             14,772,512 

; Ex 2.43
(define (louis-queens board-size)
  (define (queen-cols k)
    (if (= k 0) 
        (list empty-board) 
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens)
                                  (adjoin-position new-row k rest-of-queens)
                                  )
                                (queen-cols (- k 1))
                                )
                           )
                         (enumerate-interval 1 board-size)
                         )
                )
        )
    )
  (queen-cols board-size)
  )


; Explain why Louis's version of queens runs so slowly. Estimate the time it will take to complete if the
; program in 2.42 finishes in time T.
