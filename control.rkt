#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui
         "tock.rkt")
(provide (all-defined-out))

;; CTRL:
;; 1. check-left ✓
;; 2. check-right ✓
;; 3. move-left ✓
;; 4. move-right ✓

;; Tet Ke -> Tet
;; moves the tetrimono left or right if there is nothing blocking it's path
;; 1. "left" - moves the tetrimono one left on the grid, does not move if it is at the left border or there is another tetrimono one to the left
;; 2. "right" - moves the tetrimono one right on the grid, does not move if it is a the right border or there is another tetrimono one to the right
;; 3. "down" - moves the block to the lowest viable position on the same x
(define (control tet ke)
  (cond
    [(and (string=? ke "left") (check-left (tet-hand tet) (tet-blocks tet))) (make-tet (move-left (tet-hand tet)) (tet-blocks tet))]
    [(and (string=? ke "right") (check-right (tet-hand tet) (tet-blocks tet))) (make-tet (move-right (tet-hand tet)) (tet-blocks tet))]
    [(string=? ke "down") (move-down (tet-hand tet) (tet-blocks tet))]
    [else tet]))

;; OK
(check-expect (control CTRL-L-EX1 "left") (make-tet (make-posn 4 10) (list)))
(check-expect (control CTRL-L-EX2 "left") (make-tet (make-posn 9 10) (list (make-posn 2 10))))
(check-expect (control CTRL-R-EX1 "right") (make-tet (make-posn 6 10) (list)))
(check-expect (control CTRL-R-EX2 "right") (make-tet (make-posn 2 10) (list (make-posn 9 10))))

;; Blocked by wall:
(check-expect (control CTRL-L-EX4 "left") CTRL-L-EX4)
(check-expect (control CTRL-L-EX6 "left") CTRL-L-EX6)
(check-expect (control CTRL-R-EX4 "right") CTRL-R-EX4)
(check-expect (control CTRL-R-EX6 "right") CTRL-R-EX6)

;; Blocked from block/both sides
(check-expect (control TOCK-F-EX4 "right") TOCK-F-EX4)
(check-expect (control TOCK-F-EX4 "left") TOCK-F-EX4)

(check-expect (control CTRL-L-EX3 "left") CTRL-L-EX3)
(check-expect (control CTRL-L-EX5 "left") CTRL-L-EX5)
(check-expect (control CTRL-R-EX3 "right") CTRL-R-EX3)
(check-expect (control CTRL-R-EX5 "right") CTRL-R-EX5)

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x-1,y)or if they are on the border
;; 1. there is -> false
;; 2. there isn't -> true
;; 3. empty list -> true
(define (check-left hand blocks)
  (cond
    [(= (posn-x hand blocks) 1) #f]
    [(aux-blocked? (make-posn (- (posn-x hand) 1) (posn-y hand)) blocks) #f]
    [else #t]))

(check-expect (check-left HAND-CTRL-EX1 BLOCKS-CTRL-EX1) #t)
(check-expect (check-left HAND-CTRL-EX3 BLOCKS-CTRL-EX2) #t)
(check-expect (check-left HAND-CTRL-EX3 BLOCKS-CTRL-EX3) #f)
(check-expect (check-left HAND-CTRL-EX2 BLOCKS-CTRL-EX4) #f)
(check-expect (check-left HAND-CTRL-EX4 BLOCKS-CTRL-EX4) #f)
(check-expect (check-left HAND-CTRL-EX2 BLOCKS-CTRL-EX1) #f)

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x+1,y)
;; 1. there is -> false
;; 2. there isn't -> true
;; 3. empty list -> true
(define (check-right hand blocks)
  (cond
    [(= (posn-x hand) 10) #f]
    [(aux-blocked? (make-posn (+ (posn-x hand) 1) (posn-y hand)) blocks) #f]
    [else #t]))

(check-expect (check-right HAND-CTRL-EX1 BLOCKS-CTRL-EX1) #t)
(check-expect (check-right HAND-CTRL-EX2 BLOCKS-CTRL-EX3) #t)
(check-expect (check-right HAND-CTRL-EX2 BLOCKS-CTRL-EX2) #f)
(check-expect (check-right HAND-CTRL-EX3 BLOCKS-CTRL-EX4) #f)
(check-expect (check-right HAND-CTRL-EX4 BLOCKS-CTRL-EX4) #f)
(check-expect (check-right HAND-CTRL-EX3 BLOCKS-CTRL-EX1) #f)

;; Posn(hand) -> Posn
;; substracts 1 from posn-x and moves the block left
(define (move-left hand)
  (make-posn (- (posn-x hand) 1) (posn-y hand)))

(check-expect (move-left HAND-CTRL-EX1) (make-posn 4 10))
(check-expect (move-left HAND-CTRL-EX3) (make-posn 9 10))
(check-expect (move-left HAND-CTRL-EX4) (make-posn 2 4))

;; Posn(hand) -> Posn
;; adds 1 to posn-x and moves the block left
(define (move-right hand)
  (make-posn (+ (posn-x hand) 1) (posn-y hand)))

(check-expect (move-right HAND-CTRL-EX1) (make-posn 6 10))
(check-expect (move-right HAND-CTRL-EX2) (make-posn 2 10))
(check-expect (move-right HAND-CTRL-EX4) (make-posn 4 4))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks)-> Tet
;; Moves the tet-hand block one down if not blocked, if so sticks it to the tet-blocks
;; 1. There are no blocks on the x coordinate -> (x 1)
;; 2. There are some blocks -> Depends
(define (move-down hand blocks)
  (cond
    [(is-blocked? hand blocks) (make-tet (make-posn 5 22) (block hand blocks))]
    [else (make-tet (make-posn (posn-x hand) (- posn-y hand 1)))]))

(check-expect (move-down (tet-hand CTRL-L-EX1) (tet-blocks CTRL-L-EX1)) (make-tet (make-posn 5 9) (list)))
(check-expect (move-down (tet-hand CTRL-L-EX2) (tet-blocks CTRL-L-EX2)) (make-tet (make-posn 10 9) (list (make-posn 2 10))))
(check-expect (move-down (tet-hand CTRL-R-EX2) (tet-blocks CTRL-R-EX2)) (make-tet (make-posn 1 9) (list (make-posn  9 10))))
(check-expect (move-down (tet-hand CTRL-R-EX3) (tet-blocks CTRL-R-EX3)) (make-tet (make-posn 1 9) (list (make-posn 2 10))))
(check-expect (move-down (tet-hand CTRL-D-EX1) (tet-blocks CTRL-D-EX1)) (make-tet (make-posn 5 22) (list (make-posn 1 1))))
(check-expect (move-down (tet-hand CTRL-D-EX2) (tet-blocks CTRL-D-EX2)) (make-tet (make-posn 5 22) (list (make-posn 1 1) (make-posn 1 2))))
(check-expect (move-down (tet-hand CTRL-D-EX3) (tet-blocks CTRL-D-EX3)) (make-tet (make-posn 5 22) (list (make-posn 5 1) (make-posn 5 2) (make-posn 5 3))))

(test)


  
  
