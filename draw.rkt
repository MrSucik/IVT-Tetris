#lang racket
(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui)
(provide (all-defined-out))

;; draw:
;; 1. draw-blocks ✓
;; 2. schematic->actual ✓
;; 3. block-list ✓
;; 4. actual->schematic

(define (draw tet)
  (place-images
   (list GRID
         WHITE-SPACE)
   (list
    (make-posn HALF-SCENE-WIDTH
               HALF-SCENE-HEIGHT)
    (make-posn (half SCENE-WIDTH)
               WHITE-SPACE-Y-POS))
    (draw-blocks tet MTSC)))

;; Tet bckg -> Image
;; places blocks in accordance with schematic pos given onto bckg
(define (draw-blocks tet bckg)
  (place-images
   (block-list (+ (length (tet-blocks tet)) 1))
   (schematic->actual (append (list (tet-hand tet)) (tet-blocks tet)))
   bckg))

(check-expect (draw-blocks DB-EX1 LAYOUT) (place-images
                                           (list
                                            BLOCK
                                            BLOCK
                                            BLOCK)
                                           (list
                                            (make-posn 55 247) (make-posn 145 247)
                                            (make-posn 75 217))
                                           LAYOUT))
(check-expect (draw-blocks DB-EX2 LAYOUT) (place-images
                                           (list
                                            BLOCK
                                            BLOCK
                                            BLOCK)
                                           (list
                                            (make-posn 55 57) (make-posn 145 57)
                                            (make-posn 105 177))
                                           LAYOUT))
(check-expect (draw-blocks DB-EX3 LAYOUT) (place-images
                                           (list
                                            BLOCK
                                            BLOCK
                                            BLOCK)
                                           (list
                                            (make-posn 55 157) (make-posn 145 157)
                                            (make-posn 65 237))
                                           LAYOUT))
(check-expect (draw-blocks DB-EX4 LAYOUT) (place-images
                                           (list
                                            BLOCK
                                            BLOCK
                                            BLOCK)
                                           (list
                                            (make-posn 95 247) (make-posn 95 57)
                                            (make-posn 85 107))
                                           LAYOUT))

;; ListOf(Posn) -> ListOf(Posn)
;; converts the schematic description of posn on the board to actual applicable values
(define (schematic->actual pos)
  (map (lambda (posit)
         (make-posn (inexact->exact (+ X-OFFSET (* (posn-x posit) CUBE-LENGTH)))
                    (inexact->exact (- Y-OFFSET (* (posn-y posit) CUBE-LENGTH))))) pos))

(check-expect (schematic->actual S->A-EX1) (list (make-posn 55 247) (make-posn 145 247)))
(check-expect (schematic->actual S->A-EX2) (list (make-posn 55 57) (make-posn 145 57)))
(check-expect (schematic->actual S->A-EX3) (list (make-posn 55 157) (make-posn 145 157)))
(check-expect (schematic->actual S->A-EX4) (list (make-posn 95 247) (make-posn 95 57)))

;; Num -> ListOf(Blocks = Images)
;; Creates a list containing as many picts of BLOCKS as high is the number n
(define (block-list n)
  (for/list([x (in-range n)])
    BLOCK))
    
(check-expect (block-list BL-EX1) (list BLOCK))
(check-expect (block-list BL-EX2) (list BLOCK BLOCK BLOCK BLOCK BLOCK))
(check-expect (block-list BL-EX3) (list BLOCK BLOCK BLOCK BLOCK BLOCK BLOCK BLOCK))

(test)


