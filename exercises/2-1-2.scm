; Section 2.1.2

; Ex 2.2
; Implement a representation of line segments in a plane

; required function
(define (print-point p) 
  (newline) 
  (display "(")
  (display (x-point p)) 
  (display ",") 
  (display (y-point p)) 
  (display ")")
  )

; Segments

; Points

; Midpoint-segment

; Tests
; modify as necessary (these are meant as (x,y) = (make-point x y)
(define origin (make-point 0 0))
(define pt1 (make-point -4 7))
(define pt2 (make-point 2 5))
(define pt3 (make-point 2 -7))

(display "Testing points")
(print-point origin) 
(print-point pt1)
(print-point pt2)

(newline)
(display "Testing segments & midpoint")
(define seg_0_pt1 (make-segment origin pt1))
(define seg_pt1_pt2 (make-segment pt1 pt2))
(define seg_pt2_pt3 (make-segment pt2 pt3))
(define seg_zero_len (make-segment pt2 pt2))

(print-point (midpoint-segment seg_0_pt1)) ; (-2,3.5)
(print-point (midpoint-segment seg_pt1_pt2)) ; (-1,6) - off-origin segment
(print-point (midpoint-segment (make-segment pt2 pt1))) ; (-1,6) - reverse segment should have the same midpoint
(print-point (midpoint-segment (make-segment pt2 pt3))) ; (2,-1)  - vertical line 
(print-point (midpoint-segment seg_pt2_pt3)) ; (2,-1) - segments created the same way give the same answer
(print-point (midpoint-segment seg_zero_len)) ; (2,5) - zero-length segment

; Ex 2.3
; Implementing rectangles in a plane

; Create area and perimeter procedures


; Tests

(define (show-rect-values rect)
  (display "area:")
  (display <?area-function?>)
  (newline)
  (display "perimeter:")
  (display <?perimeter-function?>)
  (newline)
  )

(newline)
(newline)
(displayln "Testing Rectangles")
; Create rect1: 4x7 with corners at (0,0),(-4,0),(-4,7),(0,7) 

; Create rect2: 6x13 with corners at (-4,7),(2,7),(2,-6),(-4,-6) 

; Create rect3: 2x14 with corners at (1,12),(-1,12),(-1,-2),(1,-2)

; Measure the area and perimeter 
(show-rect-values rect1) ; Area - 28, perimeter - 22
(show-rect-values rect2) ; Area - 78, perimeter - 38
(show-rect-values rect3) ; Area - 28, perimeter - 32

; Does the order of arguments to the constructor matter?

; Does the constructor allow a degenerate rectangle (i.e. zero-length sides)?


; Alternate implementation
; Try to avoid needing to rewrite perimeter and area functions


; Tests
(newline)
(displayln "Testing alternate implementation of rectangles")

; Create rectangles using the same points as above (rect1a, rect2a, rect3a)

; Measure the area and perimeter to see if they are the same
; If area and perimeter have not changed, then show-rect-values does not need to change either.
(show-rect-values rect1a) ; Area - 28, perimeter - 22
(show-rect-values rect2a) ; Area - 78, perimeter - 38
(show-rect-values rect3a) ; Area - 28, perimeter - 32 

; As before - does the constructor allow a degenerate rectangle (e.g. zero-length sides), and can the arguments be re-ordered?

; Rectangle not aligned to 90 degrees.

; Does either implementation allow for rectangles like the following?
; Create rect5: square with corners at (3,0),(0,3),(-3,0),(0,-3) ; Area 18, perimeter ~17 (12 * sqrt 2)

; Create rect6: 10x5 with corners at (0,0),(8,6), (5,10), (-3,4) ; Area 50, perimeter 30

