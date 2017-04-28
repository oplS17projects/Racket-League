#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(require "PhysicsEquations.rkt")
(require "HitBoxes.rkt")
(provide update-car)
(provide currentvel)



#|
currentvel = calculates the current velocity of the object

@param max = max allowed velocity of object
@param vx  = x velocity
@param vy  = y velocity
@return    = returns a list of vx and vy so the current velocity
             is less than the max value
|#
(define (currentvel vx vy max)
  (let* ((v (findVelo (list vx vy)))
         (c (if (= v 0) 0 (acos (/ vx v)))))
    (cond [(= 0 v) (list 0 0)]
          [(> max v) (list (- vx 0.001)
                           (- vy 0.001))]
          [(> v max)(list (- (* max (cos c)) 0.001)
                          (- (* max (sin c)) 0.001))])))




(define (activate-boost num)
  "Nothing Implemented")

#|
  update-car: main procedure for updating a car.
  Deals with turning it, finding new velocity, and moving it

  Takes a car object and returns nothing
|#

(define (update-car c)
  (turn-car c)
  (accelerate-car c)
  (move-car c))

#|
  turn-car: procedure for turning a car.
  Checks if a car is supposed to be turining and adjusts
  angle accordingly

  Takes a car object and returns nothing
|#


(define (turn-car c)
  (let ((current-theta (c 'get-theta)))
    (cond ((c 'turning-left?) ((c 'update-theta) (+ current-theta 15)))
          ((c 'turning-right?) ((c 'update-theta) (- current-theta 15)))
          (else ((c 'update-theta) current-theta)))))

#|
  accelerate-car: procedure for adjusting cars velocity
  Checks if a car is accelerating or decelerating and adjusts its
  velocity accordingly. If it's neither accelerating or decelerating, it
  calls the slow-car function to decrease the cars speed

  Takes a car object and returns nothing
|#

(define (accelerate-car c)
  (let* ((theta (degrees->radians (c 'get-theta)))
        (current-vx (car (c 'get-velo)))
        (current-vy (cadr (c 'get-velo)))
        (new-vx (* .2  (cos theta)))
        (new-vy (* .2 (sin theta))))
    (cond ((and (c 'accel?) (c 'decel?)) ((c 'update-velo) (list current-vx current-vy)))
          ((c 'accel?) ((c 'update-velo) (list (+ current-vx new-vx) (- current-vy new-vy))))
          ((c 'decel?) ((c 'update-velo) (list (- current-vx new-vx) (+ current-vy new-vy))))
          (else (slow-car c)))))

#|
  slow-car: procedure that simulates friction aka drag
  Checks the velocity of the car and reduces its magnitude.
  This is very messy and can probably be optimized.

  takes a car object and returns nothing
|#

(define (slow-car c)
  (let* ((theta (degrees->radians (c 'get-theta)))
        (current-vx (car (c 'get-velo)))
        (current-vy (cadr (c 'get-velo)))
        (vx-drag (abs (* .1  (cos theta))))
        (vy-drag (abs (* .1 (sin theta)))))
    (cond ((> current-vx 0)
           (cond ((> current-vy 0) ((c 'update-velo) (list (- current-vx vx-drag) (- current-vy vy-drag))))
                 ((< current-vy 0) ((c 'update-velo) (list (- current-vx vx-drag) (+ current-vy vy-drag))))
                 (else ((c 'update-velo) (list (- current-vx vx-drag) current-vy)))))
          ((< current-vx 0)
           (cond ((> current-vy 0) ((c 'update-velo) (list (+ current-vx vx-drag) (- current-vy vy-drag))))
                 ((< current-vy 0) ((c 'update-velo) (list (+ current-vx vx-drag) (+ current-vy vy-drag))))
                 (else ((c 'update-velo) (list (+ current-vx vx-drag) current-vy)))))
          (else
           (cond ((> current-vy 0) ((c 'update-velo) (list current-vx (- current-vy vy-drag))))
                 ((< current-vy 0) ((c 'update-velo) (list current-vx (+ current-vy vy-drag))))
                 (else ((c 'update-velo) (list current-vx current-vy))))))))

#|
  move-car: procedure for updating car's position
  Uses current velocity and position to find new position.

  Takes a car object and returns nothing.
|#


(define (move-car c)
  (let ((x (c 'get-x))
        (y (c 'get-y))
        (vx (car (c 'get-velo)))
        (vy (cadr (c 'get-velo))))
    (begin ((c 'update-pos) (list (+ x vx) (+ y vy)))
           (checkPos c))))

#|
checkPos = insures the car is at a valid position

@param c = the car
|#

(define (checkPos c)
  (begin (hitWall c) (demo)))
          
