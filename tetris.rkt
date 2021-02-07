#lang racket
(require 2htdp/universe
         2htdp/image
         lang/posn
         "const+aux.rkt"
         "draw.rkt"
         "tock.rkt"
         "control.rkt"
         "end-game.rkt")



;; Tetris Guidline : https://tetris.wiki/Tetris_Guideline
;; following points are completely omitted:
;; Logo, hold (most likely), ghost piece (most likely), Garbage system, additional gamemodes,
;; most Tetris games use grid 10 wide 22 tall where rows above 20 are hidden
;; it is preferable if a sliver of 21st row is visible

;; Problem 1:
;; Create a simple tetris-like game which creates 1x1 blocks
;; on the top that fall down every clock tick. When blocks can't
;; fall no more, they fix to that place and another block appears on the top.
;; If a full row with no spaces is created, that row disappears.
;; The game ends when a block is locked above the 20th row.
;; Controls should move the blocks left/right in accordance with playfield,
;; with set boundaries.

;; Problem 2:
;; Modify your tetris game to incorporate new functions.
;; Add all of the tetrimonos and make them look as guidelines state.
;; Have in effect "random bag" generator for the 7 pieces.
;; Have the pieces spwawn in their designated location as per guidelines
;; and with the correct starting rotation (https://tetris.wiki/Super_Rotation_System)

;; Problem 3:
;; Modify your tetris game to incorporate rotation of blocks.
;; Source of all this mess: https://tetris.wiki/Super_Rotation_System
;; The source is detailed and problem solved, develop the same solution
;; to follow the guidelines.
;; Primarily the concern should be to allow the tetrimonos to use Basic Rotation.

;; Problem 4:
;; Basically Wall Kicks, still https://tetris.wiki/Super_Rotation_System
;; This is a more complex way of rotating the tetrimonos, if they are
;; obscured by a wall or another blocks.

;; Welcome additions:
;; Point system (with combos?), Lock Down, Piece preview, Ghost piece, Levels, Music
;; T-Spin (very obscure)
;;================ Wish list: ================
;; Preordained:
;; 1. draw (to-draw) ✓
;; 2. tock (on-tick) ✓
;; 3. end-game (stop-when)
;; 4. control (on-key) ✓
;; 5. main ✓

(define (tet-main tet)
  (big-bang tet
    [on-key control]
    [on-tick tock CLOCK-SPEED]
   [to-draw draw]
    [stop-when end-game?]))

(define NORMAL-START
  (tet-main (make-tet (make-posn 5 21) '())))



;; What needs to be done:
;; 1. blocks above 21 should be obstructed by something as they are drawin in function draw ✓
;; 2. finish stop-when function ✓
;; 3. find the right tick-rate ✓
;; 4. lmao forgot about row clear HOHOHO
;; 5. add connect-10 to tock and fall-down to control
