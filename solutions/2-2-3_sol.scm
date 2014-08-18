; Section 2.2.3

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

(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence)
  )

(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1)
  )

(define (length-acc sequence)
  (accumulate (lambda(x y) (+ 1 y)) 0 sequence)
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
                (+ this-coeff (* x higher-terms))
                )
              0
              coefficient-sequence)
  )

; Testing
(announce "Testing Horner's Rule evaluation")
(define constant-val (random 10)) 
(define ramp-fn (list 0 1))            ; y = x
(= 0 (horner-eval 0 ramp-fn))
(= constant-val (horner-eval (random 100) (list constant-val)))
(= 1 (horner-eval 1 ramp-fn))
(define random-x (- (random 2000) 1000))
(= random-x (horner-eval random-x ramp-fn))

(= 79 (horner-eval 2 (list 1 3 0 5 0 1)))

; Ex 2.35
; Redefine 'count-leaves' using accumlation.

(define (count-leaves t)
  (accumulate + 0 (map (lambda(x) (if (pair? x) (count-leaves x) 1)) t))
  )

; Testing
(announce "Testing count-leaves")
; See section 2.2.2 for these examples with previous version of count-leaves
(define x (cons (list 1 2) (list 3 4))) 
(length x)
(count-leaves x)
(length (list x x))
(count-leaves (list x (list x)))


; Ex 2.36
; Complete the definition of accumulate-n

(define (accumulate-n op init seqs) 
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))
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
  (map (lambda(row) (dot-product row v)) m)
  ) 

(define (transpose mat)
  (accumulate-n cons null mat)
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(row) (matrix-*-vector cols row)) m)
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
(fold-right / 1 (list 1 2 3))        ; 3/2 (1 / 1 / 2 / 3)
(fold-left / 1 (list 1 2 3))         ; 1/6   (((1 / 3)/ 2)/1)
(fold-right list null (list 1 2 3))  ; (1 (2 (3 ())))
(fold-left list null (list 1 2 3))   ; (((() 1) 2) 3)

; A commutative operation should be sufficient.
(= (fold-left + 2 (list 1 2 3)) (fold-right + 2 (list 1 2 3)))  ; for one example 

; Ex 2.39
; Complete the definitions of reverse using fold-left and fold-right.

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append (list x) y)) null sequence)
  )

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence)
  )

; Testing
(announce "Testing reverse using left and right")
(equal? (reverse-r 1-to-4) (reverse-l 1-to-4))  
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
(define (unique-pairs n)
  (flatmap (lambda(i)
             (map (lambda(j)(list i j))
                  (enumerate-interval 1 (- i 1))
                  )
             )
           (enumerate-interval 1 n)
           )
  )

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n)))
  )

; Testing
(announce "Testing unique-pairs and prime-sum-pairs")
(unique-pairs 4)
(prime-sum-pairs 6)

; Ex 2.41
; Write a procedure to generate the set of distinct ordered triples < n that sum to a given value s

(define (unique-triples n)
  (flatmap (lambda(i) 
             (map (lambda(j) 
                    (cons i j)
                    )
                  (unique-pairs (- i 1)) 
                  )
             )
           (enumerate-interval 1 n)
           )
  )

(define (triple-sum? triple sum) 
  (= sum (+ (car triple) (cadr triple) (caddr triple)))
  )
; Calling using n, s

(define (sum-triples n s)
  (filter (lambda(x) (triple-sum? x s)) (unique-triples n))
  )

(announce "Testing triples")
(sum-triples 6 9)    ; 3 triples
(sum-triples 9 57)   ; none 
(sum-triples 3 6)    ; 1 triple (1,2,3)
(sum-triples 10 14)  ; 9 triples
(sum-triples 2 2)    ; none - no triples exist for n <= 2
(sum-triples -5 1)   ; none - n need not be positive.
(sum-triples 20 -1)  ; none - s need not be positive.
(sum-triples 5 -10)  ; none - i,j,k must be positive.

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

(define board-file-names (list "a" "b" "c" "d" "e" "f" "g" "h"))
(define (to-file-letter num) (list-ref board-file-names (- num 1)))

(define (make-square f r) (cons f r))
(define (rank square) (cdr square))
(define (file square) (car square))
(define (print-square s)
  (display (to-file-letter (file s)))
  (display (rank s))
  )


(define empty-board null)

(define (adjoin-position row col rest) (cons (make-square col row) rest))

(define (first-match li proc)
  (let ((found (filter proc li)))
    (if (null? found)
        null
        (car found)
        )
    )
  )

; 
(define (safe? k positions)
  (define (check-position check-square positions)
    (if (null? positions) 
        true
        (let ((this-square (car positions)))
          (cond
            ((and (= (file this-square) (file check-square)) (= (rank this-square) (rank check-square)))
                  (check-position check-square (cdr positions)) ; Don't check this square
                  ) 
            ((= (file this-square) (file check-square)) false)
            ((= (rank this-square) (rank check-square)) false)
            ((= (abs (- (file check-square) (file this-square))) (abs (- (rank check-square) (rank this-square)))) false)
            (else (check-position check-square (cdr positions)))
            )
          )
        )
    )
  (let ((check-square (first-match positions (lambda(p) (= (file p) k)))))
    (if (null? check-square)
        true
        (check-position check-square positions)
        )
    )
  )


; Testing 

; (print-solution) is required to display a position.  (queens)
; returns a sequence of solutions.
(define (print-solution sol)
  (for-each (lambda(s) 
              (print-square s)
              (display ",")
              )
            sol)
  (if (empty? sol)
      (display "No solutions")
      )
  (newline)
  )

(announce "Testing queens")
(for-each print-solution (queens 0))
(for-each print-solution (queens 1))
(for-each print-solution (queens 2))
(for-each print-solution (queens 3))
(for-each print-solution (queens 4)) ; First case with an interesting solution
(for-each print-solution (queens 5))
(for-each print-solution (queens 6))
(for-each print-solution (queens 8))
(length (queens 8))
;(length (queens 9))
;(length (queens 12))

; Number of solutions for a given board size
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
(define call-count 0)

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

; Louis's version makes recursive calls to queen-cols for each row of the board.  In contrast, enumerate-interval
; is a much faster, iterative routine.  Placing it in the inner loop makes more sense than putting the slower
; queen-cols inside there.
;
; Louis's version calls queen-cols for each re-entry into the enumerate-interval loop.  What was one call to queen-cols
; becomes n calls for n = 1 to k.  This is O(k^k).  It's only an upper limit compared to our original version, though. The exact 
; ratio in terms of time is difficult to calculate.  What can be said is that it's *no worse* than T^N.    

; It is possible to see that the total number of calls to queen-col is in fact on the order N^N, as testing below shows it
; to be (N^(N+1)-1)/(N-1).  

(announce "Checking Louis's problem with queens puzzle")

; Verify that the solution is still correct:
(define (lt-square a b)
  (cond ((< (rank a) (rank b)) true)
        ((and (= (rank a) (rank b)) (< (file a) (file b))) true)
        (else false)
        )
  )

; Used to sort positions in a solution.
; p1 and p2 are assumed sorted by square
(define (lt-position p1 p2)
    (cond ((null? p1) true)
          ((null? p2) false)
          ((lt-square (car p1) (car p2)) true)
          ((equal? (car p1) (car p2)) (lt-position (cdr p1) (cdr p2)))
          (else false)
          )
    )

(define (sort-position p)
  (sort p lt-square)
  )

(define (sort-sol s)
  (sort (map sort-position s) 
        lt-position
        )
  )
  

(displayln "Verifying correct solutions:")
(equal? (sort-sol (queens 4)) (sort-sol (louis-queens 4)))
(equal? (sort-sol (queens 5)) (sort-sol (louis-queens 5)))
(equal? (sort-sol (queens 6)) (sort-sol (louis-queens 6)))

(displayln " Board size         Procedure    Time to process")
(display   "     5               (queens):   ")
(time (void (queens 5)))
(display   "     5         (louis-queens):   ")
(time (void (louis-queens 5)))

(display   "     6               (queens):   ")
(time (void (queens 6)))
(display   "     6         (louis-queens):   ")
(time (void (louis-queens 6)))

(display   "     7               (queens):   ")
(time (void (queens 7)))
;(display   "     7         (louis-queens):   ")
;(time (void (louis-queens 7)))

(define (louis-queens board-size)
  (define (queen-cols k)
    (set! call-count (add1 call-count)) ; added for monitoring
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

(displayln "Testing that (louis-queens) still correct after adding counter:")
(equal? (sort-sol (queens 4)) (sort-sol (louis-queens 4)))
(equal? (sort-sol (queens 5)) (sort-sol (louis-queens 5)))


(define call-count 0)
(displayln "Counting calls in louis-queens.")
(display "Time for board size 6:")
(time (void (louis-queens 6)))   ; cpu time: 540 real time: 540 gc time: 0
(display " call-count is ")      ; 55987
(displayln call-count)


;(time (void (queens 7)))        ; cpu time: 125 real time: 125 gc time: 0
;(time (void (louis-queens 7)))  ; cpu time: 12741 real time: 13160 gc time: 2432
;(display " call-count is ")     ; 960800
;(displayln call-count)

;(time (void (queens 8)))  ; cpu time: 1881 real time: 1886 gc time: 99
;(time (void (louis-queens 8))) ;cpu time: 222313 real time: 226460 gc time: 20106
;(display " call-count is ")  ; 19173961
;(displayln call-count)


  