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
    ((ball 'update-ball) (ballHitCar car1))
    ((ball 'update-ball) (ballHitCar car2))
    ((ball 'update-ball) (list (list (car nXpos) (car nYpos))
                                   (list (cadr nXpos) (cadr nYpos))))))

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
         (T (list bx (- by 15)))  #|ball T pos|#
         (B (list bx (+ by 15)))  #|ball B pos|#
         (R (list (+ bx 15) by))  #|ball R pos|#
         (L (list (- bx 15) by))  #|ball L pos|#
         (hb (carHitBox ca)) #|car hit box|#
         (E (car hb))        #|pt E car|#
         (F (cadr hb))       #|pt F car|#
         (G (caddr hb))      #|pt G car|#
         (H (cadddr hb)))    #|pt H car|#
    (cond [(and (ptG B E) (ptG G B))  #|Is B inside|#
           (list T (list vx (- vy)))]
          [(and (ptG T E) (ptG G T))  #|Is T inside|#
           (list B (list vx (- vy)))]
          [(and (ptG R E) (ptG G R))  #|Is R inside|#
           ((ball 'update-ball) (list L (list (- vx) vy)))]
          [(and (ptG L E) (ptG G L))  #|Is L inside|#
           (list R (list (- vx) vy))]
          [else (list (list bx by) (list vx vy))])))

#|
update = updates positions and velocities

@param lst = list of everything
@return    = updated everything
|#

(define (update w)
  ((car1 'update-car) (append (carPos (car1 'get-pos) (car1 'get-velo)) (list (car1 'get-theta))))
  ((car2 'update-car) (append (carPos (car2 'get-pos) (car2 'get-velo)) (list (car2 'get-theta))))
  (ballPos (ball 'get-pos) (ball 'get-velo) 15)
  w)
