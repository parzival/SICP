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
(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

; Points
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (average x y) 
  (/ (+ x y) 2)
  )

; Midpoint-segment
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))
              )
  )

; Tests
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

; Note that the area and perim implementations use the (rather unnecessary) concept of a rectangle requiring a 'length' and a 'width'.  Each implementation holds to this, although for these functions it would suffice merely to give a different side than was previously asked for.  

; This uses two corners (points) to describe a rectangle.
(define (make-rect c1 c2)
  (cons c1 c2)
  )

(define (corner-1 r) (car r))
(define (corner-2 r) (cdr r))

(define (rect-length r)
  (let ((side-X (abs (- (x-point (corner-1 r)) (x-point (corner-2 r)))))
        (side-Y (abs (- (y-point (corner-1 r)) (y-point (corner-2 r)))))
        )
    (if (> side-Y side-X)
        side-Y
        side-X
        )
    )
  )

(define (rect-width r)
  (let ((side-X (abs (- (x-point (corner-1 r)) (x-point (corner-2 r)))))
        (side-Y (abs (- (y-point (corner-1 r)) (y-point (corner-2 r)))))
        )
    (if (< side-Y side-X)
        side-Y
        side-X
        )
    )
  )


; Create area and perimeter procedures
(define (rect-perim rect)
  (+ (* 2 (rect-length rect))
     (* 2 (rect-width rect))
     )
  )

(define (rect-area rect)
  (* (rect-length rect) (rect-width rect))
  )

; Tests

(define (show-rect-values rect)
  (display "area:")
  (display (rect-area rect))
  (newline)
  (display "perimeter:")
  (display (rect-perim rect))
  (newline)
  )

(newline)
(newline)
(displayln "Testing Rectangles")
; Create rect1: 4x7 with corners at (0,0),(-4,0),(-4,7),(0,7) 
(define rect1 (make-rect origin pt1))

; Create rect2: 6x13 with corners at (-4,7),(2,7),(2,-6),(-4,-6) 
(define rect2 (make-rect pt1 (make-point 2 -6)))

; Create rect3: 2x14 with corners at (1,12),(-1,12),(-1,-2),(1,-2) 
(define rect3 (make-rect (make-point -1 -2) (make-point 1 12)))

; Measure the area and perimeter 
(show-rect-values rect1) ; Area - 28, perimeter - 22
(show-rect-values rect2) ; Area - 78, perimeter - 38
(show-rect-values rect3) ; Area - 28, perimeter - 32 

; Does the order of arguments to the constructor matter?
; Create rect2_alt with a different order of arguments
(define rect2_alt (make-rect (make-point 2 -6) pt1))

(show-rect-values rect2_alt) ; should be the same as rect2

; Does the constructor allow a degenerate rectangle (i.e. zero-length sides)?
(define rect_pt (make-rect origin origin))
(define rect_line (make-rect pt2 pt3))

(displayln "Testing degenerate rectangles (point, line)")
(show-rect-values rect_pt)
(show-rect-values rect_line)

; Alternate implementation
; Try to avoid needing to rewrite perimeter and area functions

; This defines rectangles in terms of two segments with a common origin.  In this case, the long side and short
; side are determined only once, when the rectangle is created and stored as segments. 

(define (seg-length seg)
  (sqrt (+ (sqr(- (x-point (start-segment seg)) (x-point (end-segment seg))))
           (sqr(- (y-point (start-segment seg)) (y-point (end-segment seg))))
           )
        )
  )

(define (make-rect x-seg y-seg)
   (let ((x-len (seg-length x-seg))
         (y-len (seg-length y-seg))
         )
      (if (> x-len y-len)
          (cons x-seg y-seg)
          (cons y-seg x-seg)
          )
    )
  )

(define (rect-length r) 
  (seg-length (car r))
  )

(define (rect-width r) 
  (seg-length (cdr r))
  )

; Tests
(newline)
(displayln "Testing alternate implementation of rectangles")

(define r2leg1 (make-segment pt1 (make-point 2 7)))
(define r2leg2 (make-segment pt1 (make-point -4 -6)))

; Create rectangles using the same points as above (rect1a, rect2a, rect3a)
(define rect1a (make-rect (make-segment origin (make-point -4 0))
                          (make-segment origin (make-point  0 7))
                          )
  )
(define rect2a (make-rect r2leg1 r2leg2))
(define rect2_alta (make-rect r2leg2 r2leg1))
(define r3orig (make-point 1 12))
(define rect3a (make-rect (make-segment r3orig (make-point 1 -2))
                          (make-segment (make-point -1 12) r3orig)
                          )
  )


; Measure the area and perimeter to see if they are the same
(show-rect-values rect1a) ; Area - 28, perimeter - 22
(show-rect-values rect2a) ; Area - 78, perimeter - 38
(show-rect-values rect2_alta) ; should be the same as rect 2
(show-rect-values rect3a) ; Area - 28, perimeter - 32

(displayln "Testing degenerate rectangles (point, line)")
(define zero-seg (make-segment origin origin))
(define rect_pt (make-rect zero-seg zero-seg))
(define rect_line (make-rect (make-segment pt2 pt2)
                             (make-segment pt2 pt3)
                             )
  )
(show-rect-values rect_pt)
(show-rect-values rect_line)

; Rectangle not aligned to 90 degrees.

; Create rect5: square with corners at  (3,0),(0,3),(-3,0),(0,-3)
(define rhomb-origin (make-point -3 0))
(define rect5 (make-rect (make-segment rhomb-origin (make-point 0 3))
                         (make-segment rhomb-origin (make-point 0 -3))
                         )
  )
; Create rect6: 10x5 with corners at (0,0),(8,6),(5,10),(-3,4)
(define rect6 (make-rect (make-segment origin (make-point 8 6))
                         (make-segment origin (make-point -3 4))
                         )
  )

; Parallelogram, but not a rectangle: corners at (0,0),(-4,7),(-2, 12),(2,5)
(define parallelo (make-rect (make-segment origin pt1) 
                             (make-segment origin pt2)
                             )
  )

(displayln "Testing rotated rectangles")
(show-rect-values rect5) ; Area - 18, perimeter - ~16.97 (12 * sqrt 2)
(show-rect-values rect6) ; Area - 50, perimeter - 30
(show-rect-values parallelo) ; True area is 34 (this gets it wrong), perimeter - ~26.9