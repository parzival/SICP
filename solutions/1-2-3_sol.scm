; Section 1.2.3

; Setup functions
(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1)
                     )
                 (cc (- amount (first-denomination kinds-of-coins)) 
                     kinds-of-coins
                     )
                 )
              )
        )
  )

(define (first-denomination kinds-of-coins) 
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10) 
        ((= kinds-of-coins 4) 25) 
        ((= kinds-of-coins 5) 50)
        )
  )

; Demonstrating count-change
(count-change 100)

; Ex 1.14 
; Showing the tree for a procedure

; Show the tree that results from calling 
(count-change 11)

; State the orders of growth for this procedure in space and time.

; In the following, n = amount, k = number of coins.  Generally it is assumed that n > k, since this represents the
; likely practical interpretation of these values.

; Orders of growth for the space

; If each call is evaluated all the way until it returns a single value, it can be seen from the tree that
; the space required will be O(depth of the tree).  The maximum depth of the tree grows linearly with n, which
; means this is O(n).  

; Order of growth for time

; The time taken is considered proportional to the calls to cc.  This is the total number of nodes in the tree.
;
; Consider a 'worst-case' run that includes every possible value, checking every possible coin set for all amounts
; from 0 to n.
;
; Here is one way to figure the ways to make change when all 'possible combinations' are valid:  
; Take a 'degenerate' set of currencies where each value = 1.  Each change combination could be expressed using an
; (n+k-1)-length sequence. A * indicates one coin, a space _ means switch to another coin type.  For instance, if
; there are three coins, **_*****_* would mean 2 of coin type 1, 5 of coin type 2, and 1 of coin type 3, giving change
; summing to 8. In this system there will be n *'s always appearing. This result can be calculated directly as C(n+k-1,n), 
; with C as the choose function.  (This is also known as "Combinations with repetition").
; 
; It takes a little more work to show the total number of calls required. The tree for this procedure has a very
; high number of repeated nodes.  In fact, it can be written as overlapping trees fitting into a modified parallelogram :
; Start with a parallelogram with the right side of length n (representing decreasing amounts)
; and a connected side of length k (representing decreasing kinds-of-coins).   
; At the ends of this parallelogram will be a line of failures on the lower left, and a line of successes on the lower right.
; Every node in the interior (not a failure or success) will appear in the proper tree position, and it will
; be called by each node that appears above it both to the left and to the right.  The number of calls to each interior 
; node is then the sum of the calls to the two nodes above it.
; Since this starts with 1 at the top and along the sides, we have a portion of Pascal's Triangle.
;
; Using a property of binomial coefficients (known as the 'hockey stick rule' due to the shape in Pascal's triangle)
; we know that the sum of any diagonal is just the value below and one move in the opposite direction of the diagonal.
; That is, for a diagonal running from the upper left to the lower right, the sum will be the value in the row below the
; last value and just to the left.
;
; Entries in Pascal's triangle can be calculated directly, again using the choose function.  For a given row and offset in
; that row, the entry is C(row,offset).
; In terms of these values the hockey stick rule can be stated as: Sum (i=0->m)  C(r+i,i) = C(r+m+1,m).
; 
; Using this property, we can calculate the number of calls that do not immediately terminate.  This the sum of all values in
; our section of Pascal's Triangle. It can be seen that the sum of calls to the nodes in each line in the parallelogram 
; equals the entry at the end of the line just "below" it to the side, with the sum of the last line equal to the value 
; in the next row of Pascal's Triangle.  These sums form another diagonal, or nearly one.  It's just missing the 
; first value (1) in the outermost row.
; Ignoring that for now, let's figure the sum of this diagonal, using the n-length side for summing. (Either side works.)

; It starts at the end of the parallelogram (row n-1 in Pascal's Triangle) and runs to row n-1+k, keeping the offset n.
; Thus the sum, using the hockey stick property, is C(n+k,k) = C(n+k,n+k-k) = C(n+k,n).  This number is the 
; total number of calls to cc that do not terminate immediately.
; 
; Now we find the total calls that terminate.  The successes will come from calls at the lower right edge, and there will
; be just one for each call to its upper left.  This just copies the leg of the parallelogram. Similarly, all the calls that end
; on the lower left (failures) will occur once for each call on its upper right.
; Using the hockey stick property, the total number of terminating calls will be the value one below and to the left of our 
; parallelogram (for failures), and one below and to the right of our parallelogram (for successes).
;
; Those two values are in fact in the triangle just above the sum we figured for non-terminating calls.  Thus, the number
; of successes and the number of failures sum to that same number.

; This means that the total number of calls is:

; 2*C(n+k,n) - 1. 

; (We had to make sure not to forget to subtract that 1).

; This process also can be used to find the total number of change combinations (calls that result in success).  It is the
; entry C(n+k-1,n), in the row just above our total value.  This is the same as computed previously.

; Here's a digression to show a simple expression for the total calls in terms of the successful ones.  This may
; also relate this to similar demonstrations of the time complexity of this procedure.

; If we divide the value for the total terminations by the number of successes, we get:

; C(n+k,n)
; --------
; C(n+k-1,n)
;
; (n+k)!/((n+k-n)!n!)
; ----------------------
; (n+k-1)!/((n+k-1-n)!n!
;
; (n+k)!(k-1)!
; -------------
; (n+k-1)!k!
;
; (n+k)
; -----
;   k
;
; (n/k + 1)
;
; Therefore an alternate expression for the total calls is (2*(n/k + 1)*(cc n k)) - 1),
; where cc is our procedure (and it is evaluated in this expression).


; I have claimed (without proof) that this is a "worst-case" result. A procedure with unique
; positive integer values for currencies will take a number of steps that are some fraction of this value.
; If fractional coin values were possible, the values can be normalized and will only differ
; by a constant in terms of time taken.  The claim, then, is that if this case is O(f) then all sets of coins
; will complete in O(f) time.

; A simpler bound can be found by observing that the expression is equal to -1 + 2*(n+k)*(n+k-1)*(n+k-2)*...*(n+1)/k!.
; The growth of this is determined by the fraction's numerator, which will be a polynomial in n 
; having 2*n^k as its highest term. Thus, the time taken increases at a rate no worse than n^k. 
; Written using Big-O notation the procedure is O(n^k).

; Testing, using the set described above

(define count 0)

; Instead of solving, returns the number of times it was called
(define (cc-counter amount kinds-of-coins)
  (cond ((= amount 0) 1)  ; success
        ((or (< amount 0) (= kinds-of-coins 0)) 1) ; failure
        (else (+ (cc-counter amount
                     (- kinds-of-coins 1)
                     )
                 (cc-counter (- amount 1) ; All denoms assumed to be 1
                     kinds-of-coins
                     )
                 1 ; non-terminating
                 )
              )
        )
  )

(define (first-denomination kinds-of-coins) 1) ; All denoms assumed to be 1

(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))) ))

(define (test-cc-order n k)
  (display "Testing (")(display n)(display ",")(display k)(display ")") (newline)
  (display "Answer is :") (display (cc n k) ) (newline)
  (display "Call count is:") (display (cc-counter n k)) (newline)     
  (display "Count as a fraction of n^k :") (display (exact->inexact (/ (cc-counter n k) (expt n k)))) (newline)
  )

(test-cc-order 27 5) ; 31!/(4!27!) = 31465
(display "Computed count value : ")
; 2*(27/5 +1)*31465-1 = 402751
(- (* 2 (+ (/ 27 5)  1) (cc 27 5)) 1) ; computed count value
(newline)

(test-cc-order 40 5)  ; 44!/(4!40!) = 135751   ; This may take several seconds
(newline)

(test-cc-order 50 5)   ; 54!/(4!50!) = 316251   ; This may take several seconds
(newline)

(test-cc-order 15 7) ; 21!/(6!15!) = 54264
(newline)

(test-cc-order 20 7) ; 26!/(6!20!) = 230230   
(newline)

(test-cc-order 30 7)    ; 36!/(6!30!) = 1947792  ; This may take many seconds
(newline)

; Ex 1.15
; Characterizing order of procedure

(define (cube x)
  (* x x x)
  )

(define (p x)
  (display "applying p")  ; Added lines to show
  (newline)               ; the answer to 1.15a.
  (- (* 3 x) (* 4 (cube x)))
  )

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))
      )
  )

; a. Find the number of times p is called when calculating (sine 12.15).

(sine 12.15)  ; ans: 5 times.

; b. State the order of growth in space and time used to calculate (sine a).

; Each call generates a single sub-call to (sine a/3) until it is small enough.
; Therefore the time and space taken will grow at the same rate.

; Since we divide by 3 each time, we are using the fact that any number a can be 
; expressed as eps*3^x, where eps is a value considered small enough to terminate.
; It should be clear that ceil(x) is the number of divisions and thus the number of
; calls to the procedure sine. Solving for x:
;
;      a = eps*3^x,
;  log3a = log3(eps*3^x)     ; take the log of both sides (log3 means 'logarithm base 3')
;  log3a = log3(eps) + log3(3^x)
;  log3a - log3(eps) = x
;  log3(a/eps)       = x 
;  
;  Since eps is at most 0.1 (in the current procedure), the value of ceil(log3(a/0.1))
;  gives us the number of steps and thus the order in space and time. 
;  For example, for a = 12.15 this is ceil(4.369), or 5.
; 
;  Dispensing with the minimum value for order calculation, this grows at no greater rate than
;  log a regardless of the base.  For comparison's sake we can say then that this procedure 
;  is O(log a). 


