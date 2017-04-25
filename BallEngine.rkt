#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(require "PhysicsEngine.rkt")
(require "HitBoxes.rkt")
(provide ballPos)
(provide update)

#|Ball physics.  Similar to car, but bounces off edges|#

#|
ballPos = Set the ball's new position

@param pos = the position of the ball
@param vel = the velocity of the ball
@param rad = the radius of the ball
|#
(define (ballPos pos vel rad)
  (let* ((nvel (currentvel (car vel) (cadr vel) 75))          #|Sets balls velocity|#
         (nXpos (ballNewPos (car pos) (car nvel) rad 'x))     #|Finds x pos and vel|#
         (nYpos (ballNewPos (cadr pos) (cadr nvel) rad 'y)))  #|Finds y pos and vel|#
    ((ball 'update-ball) (list (list (car nXpos) (car nYpos)) #|Updates ball|#
                                   (list (cadr nXpos) (cadr nYpos)))))
  (ballHitCar car1)     #|Checks for colission with car 1|#
  (ballHitCar car2))    #|Checks for colission with car 2|#

#|
ballNewPos = calculates new ball x position

@param pos = position
@param vel = velocity
@param rad = ball radius
@return    = new position and velocity
|#
(define (ballNewPos pos vel rad axis)
  (cond [(>= 0 (- pos rad)) (list (- pos vel) (- vel))]
        [(<= (getAxis axis) (+ pos rad)) (list (- pos vel) (- vel))]
        [else (list (+ pos vel) vel)]))

#|
ballHitCar = ball colssion with car

@param ca = which car

ca
c1       ball
E     F     T
+-----+     /\
|     |  L <  > R
+-----+     \/
H     G      B     
|#

(define (ballHitCar ca)
  (let* ((bx (ball 'get-x))  #|ball x pos|#
         (by (ball 'get-y))  #|ball y pos|#
         (vx (car (ball 'get-velo)))  #|ball x velo|#
         (vy (cadr (ball 'get-velo))) #|ball y velo|#
         (U (list bx (- by 15)))  #|ball U pos|#
         (D (list bx (+ by 15)))  #|ball D pos|#
         (R (list (+ bx 15) by))  #|ball R pos|#
         (L (list (- bx 15) by))  #|ball L pos|#
         (hb (carHitBox ca)) #|car hit box|#
         (E (car hb))        #|pt E car|#
         (F (cadr hb))       #|pt F car|#
         (G (caddr hb))      #|pt G car|#
         (H (cadddr hb)))    #|pt H car|#
    (cond [(and (ptG D E) (ptG G D))  #|Is D inside|#
           ((ball 'update-ball) (list (list bx (- by vy))
                                      (list vx (- vy)) 15)) (list "D")]
          [(and (ptG U E) (ptG G U))  #|Is U inside|#
           ((ball 'update-ball) (list (list bx (- by vy))
                                      (list vx (- vy)) 15)) (list "U")]
          [(and (ptG R E) (ptG G R))  #|Is R inside|#
           ((ball 'update-ball) (list (list (- bx vx) by)
                                      (list (- vx) vy) 15)) (list "R")]
          [(and (ptG L E) (ptG G L))  #|Is L inside|#
           ((ball 'update-ball) (list (list (- bx vx) by)
                                      (list (- vx) vy) 15)) (list "L")]
          [else ((ball 'update-ball) (list (list bx by)
                                           (list vx vy) 15)) (list "none")])))

#|
update = updates positions and velocities

@param lst = list of everything
@return    = updated everything
|#

(define (update w)
  (begin
    (accelerate-car)
    (slow-car)
    (right-turn)
    (left-turn)
    (if (demo) "Demo"
        (begin ((car1 'update-car) (append (carPos car1) (list (car1 'get-theta))))
               ((car2 'update-car) (append (carPos car2) (list (car2 'get-theta))))))
    (ballPos (ball 'get-pos) (ball 'get-velo) 15)
    w))
