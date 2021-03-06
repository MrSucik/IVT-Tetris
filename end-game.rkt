#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui)
(provide (all-defined-out))

;; end-game:
;; 1. top-off? ✓

;; Tet -> Bool
;; ends the game, if blocks reach y = 21 or 21 <= y
;; 1. Ends game => y => 21 #t
;; 2. Is ok => #f
(define (end-game? tet)
  (cond
    [(top-off? (tet-blocks tet)) #t]
    [else #f]))

;; Nothing #f
(check-expect (end-game? (make-tet (make-posn 5 21) (list))) #f)
(check-expect (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 20) (make-posn 4 20)))) #f)
;; End-game #t
(check-expect (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 21)))) #t)
(check-expect (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 23)))) #t)

;; ListOf(Posn)(tet-blocks) -> Bool
;; 1. y => 21 #t
;; 2. else #f
(define (top-off? blocks)
  (cond
    [(empty? blocks) #f]
    [(or (posn>=21? (first blocks))
         (top-off? (rest blocks))) #t]
    [else #f]))

(check-expect (top-off? (list)) #f)
(check-expect (top-off? (list (make-posn 5 21))) #t)
(check-expect (top-off? (list (make-posn 5 22))) #t)

;; Posn(posn-y(tet-blocks)) -> Bool
;; returns true, if y is >= 21
(define (posn>=21? pos)
  (>= (posn-y pos) 21))

(test)

