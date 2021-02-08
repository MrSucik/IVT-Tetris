#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../tock.rkt"
         "../const+aux.rkt")


;; tock:

;; falls:
(define TOCK-F-EX1 (make-tet (make-posn 5 21) (list)))
(define TOCK-F-EX2 (make-tet (make-posn 5 3) (list (make-posn 5 1))))
(define TOCK-F-EX3 (make-tet (make-posn 1 10) (list (make-posn 1 8))))
;; Very useful for ke handler!!!
(define TOCK-F-EX4 (make-tet (make-posn 5 3)
                             (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1))))

;; is blocked:
(define TOCK-B-EX1 (make-tet (make-posn 1 2) (list (make-posn 1 1))))
(define TOCK-B-EX2 (make-tet (make-posn 5 1) (list)))
(define TOCK-B-EX3 (make-tet (make-posn 1 1) (list)))
(define TOCK-B-EX4 (make-tet (make-posn 5 15) (list (make-posn 5 14))))
(define TOCK-B-EX5 (make-tet (make-posn 10 2) (list (make-posn 10 1))))

;;tock:
(check-expect (tock TOCK-F-EX1) (make-tet (make-posn 5 20) (list)))
(check-expect (tock TOCK-F-EX2) (make-tet (make-posn 5 2) (list (make-posn 5 1))))
(check-expect (tock TOCK-F-EX3) (make-tet (make-posn 1 9) (list (make-posn 1 8))))
(check-expect (tock TOCK-F-EX4) (make-tet (make-posn 5 2) (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1))))
(check-expect (tock TOCK-B-EX1) (make-tet (make-posn 5 22) (list (make-posn 1 1) (make-posn 1 2))))
(check-expect (tock TOCK-B-EX2) (make-tet (make-posn 5 22) (list (make-posn 5 1))))
(check-expect (tock TOCK-B-EX3) (make-tet (make-posn 5 22) (list (make-posn 1 1))))
(check-expect (tock TOCK-B-EX4) (make-tet (make-posn 5 22) (list (make-posn 5 14) (make-posn 5 15))))
(check-expect (tock TOCK-B-EX5) (make-tet (make-posn 5 22) (list (make-posn 10 1) (make-posn 10 2))))

;; is-blocked?
(check-expect (is-blocked? (tet-hand TOCK-F-EX1) (tet-blocks TOCK-F-EX1))
              #f)
(check-expect (is-blocked? (tet-hand TOCK-F-EX2) (tet-blocks TOCK-F-EX2))
              #f)
(check-expect (is-blocked? (tet-hand TOCK-F-EX3) (tet-blocks TOCK-F-EX3))
              #f)
(check-expect (is-blocked? (tet-hand TOCK-F-EX4) (tet-blocks TOCK-F-EX4))
              #f)
(check-expect (is-blocked? (tet-hand TOCK-B-EX1) (tet-blocks TOCK-B-EX1))
              #t)
(check-expect (is-blocked? (tet-hand TOCK-B-EX2) (tet-blocks TOCK-B-EX2))
              #t)
(check-expect (is-blocked? (tet-hand TOCK-B-EX3) (tet-blocks TOCK-B-EX3))
              #t)
(check-expect (is-blocked? (tet-hand TOCK-B-EX4) (tet-blocks TOCK-B-EX4))
              #t)
(check-expect (is-blocked? (tet-hand TOCK-B-EX5) (tet-blocks TOCK-B-EX5))
              #t)

;; aux-blocked?
(check-expect (aux-blocked? (make-posn 0 0) (list)) #f)
(check-expect (aux-blocked? (make-posn 0 0) (list (make-posn 0 0))) #t)
(check-expect (aux-blocked? (make-posn 0 0) (list (make-posn 1 1) (make-posn 1 2) (make-posn 0 0))) #t)

;; posn-equal?
(check-expect (posn-equal? (make-posn 1 1) (make-posn 1 1)) #t)
(check-expect (posn-equal? (make-posn 0 1) (make-posn 1 1)) #f)
(check-expect (posn-equal? (make-posn -23 10) (make-posn -23 10)) #t)

;;fall?
(check-expect (fall (tet-hand TOCK-F-EX1)) (make-posn 5 20))
(check-expect (fall (tet-hand TOCK-F-EX2)) (make-posn 5 2))
(check-expect (fall (tet-hand TOCK-F-EX3)) (make-posn 1 9))
(check-expect (fall (tet-hand TOCK-F-EX4)) (make-posn 5 2))

;;block
(check-expect (block (tet-hand TOCK-B-EX1) (tet-blocks TOCK-B-EX1)) (list (make-posn 1 1) (make-posn 1 2)))
(check-expect (block (tet-hand TOCK-B-EX2) (tet-blocks TOCK-B-EX2)) (list (make-posn 5 1)))
(check-expect (block (tet-hand TOCK-B-EX3) (tet-blocks TOCK-B-EX3)) (list (make-posn 1 1)))
(check-expect (block (tet-hand TOCK-B-EX4) (tet-blocks TOCK-B-EX4)) (list (make-posn 5 14) (make-posn 5 15)))
(check-expect (block (tet-hand TOCK-B-EX5) (tet-blocks TOCK-B-EX5)) (list (make-posn 10 1) (make-posn 10 2)))

;; clear-row?

(check-expect (clear-row? #t #t) #t)