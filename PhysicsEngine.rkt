#lang racket
#|For testing|#
(define winSize (list 200 400))

(include "classes.rkt")
(include "VisualHandler.rkt")
(include "soundengine.rkt")

#|Creates new car position (pt mass)|#

#|
carpos = the new position of the car

@param pos = (x,y) position of the car
@param vel = (vx,vy) velocity of the car as components
@return    = returns a list consisting of the new position of the car
             and the new velocity
|#
(define (carpos pos vel)
  (let* ((nvel (currentvel (car vel) (cadr vel) 50))
         (nXpos (newPos (car pos) (car nvel) 'x))
         (nYpos (newPos (cadr pos) (cadr nvel) 'y)))
    (list (list nXpos nYpos) nvel)))

#|
newPos = changes a position based on a velocity

@param p  = position
@param vp = velocity component
@param maxWin = width of axis
@return   = new position
|#
(define (newPos p vp axis)
  (define (calc)
    (+ p vp))
  (cond [(> 0 (calc)) 0]
        [(> (calc) (getAxis axis)) (getAxis axis)]
        [else (calc)]))

#|Getter for a particular axis' length|#
(define (getAxis axis)
  (if (equal? axis 'x) (car winSize)
      (cadr winSize)))

#|
currentvel = calculates the current velocity of the object

@param max = max allowed velocity of object
@param vx  = x velocity
@param vy  = y velocity
@return    = returns a list of vx and vy so the current velocity
             is less than the max value
|#
(define (currentvel vx vy max)
  (let* ((v (sqrt (+ (* vx vx) (* vy vy))))
         (c (acos (/ vx v))))
    (if (> max v) (list (- vx 0.001) (- vy 0.001))
        (list (- (* max (cos c)) 0.001)
              (- (* max (sin c)) 0.001)))))

#|Convert Keyboard input to car rotation / movement|#

#|
turnLeft


|#

#|Ball physics.  Similar to car, but bounces off edges|#

#|
ballPos = Set the ball's new position

@param pos = the position of the ball
@param vel = the velocity of the ball
@param rad = the radius of the ball
@return    = List with car position and cdr velocity
|#
(define (ballPos pos vel rad)
  (let* ((nvel (currentvel (car vel) (cadr vel) 75))
         (nXpos (ballNewPos (car pos) (car nvel) rad 'x))
         (nYpos (ballNewPos (cadr pos) (cadr nvel) rad 'y)))
    (list (list nXpos nYpos) nvel)))

#|
ballNewPos = calculates new ball x position

@param pos = position
@param vel = velocity
@param rad = ball radius
@return    = new position and velocity
|#
(define (ballNewPos pos vel rad axis)
  (cond [(= 0 (- pos rad)) (list (- pos vel) (- vel))]
        [(and (equal? axis 'x)(= (car winSize) (+ pos rad))) (list (- pos vel) (- vel))]
        [(and (equal? axis 'y)(= (cdr winSize) (+ pos rad))) (list (- pos vel) (- vel))]
        [else (list (+ pos vel) vel)]))

