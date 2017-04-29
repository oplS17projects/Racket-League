#lang racket

;; Racket Libraries

;; Custom Libraries
(require "../Classes/classes.rkt")
(require "../VisualEngine/VisualHandler.rkt")
(require "../SoundEngine/soundengine.rkt")
(require "PhysicsEngine.rkt")
(require "PhysicsEquations.rkt")
(require "HitBoxes.rkt")

;; Provides
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
  (begin
    (let* ((nvel (currentvel (car vel) (cadr vel) 75))          #|Sets balls velocity|#
           (nXpos (ballNewPosX (car pos) (cadr pos) (car nvel) rad))     #|Finds x pos and vel|#
           (nYpos (ballNewPosY (cadr pos) (cadr nvel) rad)))  #|Finds y pos and vel|#
      (if (pair? nXpos)
          ((ball 'update-ball) (list (list (car nXpos) (car nYpos)) #|Updates ball|#
                                     (list (cadr nXpos) (cadr nYpos))))
          (reset)))
    (ballHitCar car1)      #|Checks for colission with car 1|#
    (ballHitCar car2)))    #|Checks for colission with car 2|#

#|
ballNewPosX = calculates new ball x position

@param pos = position
@param vel = velocity
@param rad = ball radius
@return    = new position and velocity
|#
(define (ballNewPosX x y vel rad)
  (cond [(and (>= 0 (- x rad))           #|ball is off to the left|#
              (and (< y 475) (> y 275))) #|ball is in the goal area|#
         (begin (world-state 'right-score) "goal")]
        [(>= 0 (- x rad)) (list (- x vel) (- vel))]
        [(and (<= (getAxis 'x) (+ x rad))#|ball is off to the right|#
              (and (< y 475) (> y 275))) #|ball is in the goal area|#
         (begin (world-state 'left-score) "goal")]
        [(<= (getAxis 'x) (+ x rad)) (list (- x vel) (- vel))]
        [else (list (+ x vel) vel)]))


#|
ballNewPosY = calculates new ball y position

@param pos = position
@param vel = velocity
@param rad = ball radius
@return    = new position and velocity
|#
(define (ballNewPosY pos vel rad)
  (cond [(>= 0 (- pos rad)) (list (- pos vel) (- vel))]
        [(<= (getAxis 'y) (+ pos rad)) (list (- pos vel) (- vel))]
        [else (list (+ pos vel) vel)]))

#|
ballHitCar = ball colssion with car

@param ca = which car

ca
c1       ball
E     F     U
+-----+  UL /\ UR
|     |  L <  > R
+-----+  DL \/ DR
H     G      D     
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
         (sqX (* (sqrt 2) bx))    #|ball x for mid positions|#
         (sqY (* (sqrt 2) by))    #|ball y for mid positions|#
         (UR (list (- bx sqX) (+ by sqY)))  #|ball UR pos|#
         (UL (list (- bx sqX) (- by sqY)))  #|ball UL pos|#
         (DL (list (+ bx sqX) (- by sqY)))  #|ball DL pos|#
         (DR (list (+ bx sqX) (+ by sqY)))  #|ball DR pos|#
         (hb (carHitBox ca)) #|car hit box|#
         (E (car hb))        #|pt E car|#
         (F (cadr hb))       #|pt F car|#
         (G (caddr hb))      #|pt G car|#
         (H (cadddr hb))     #|pt H car|#
         (cvx (/ (car (ca 'get-velo)) 2))   #|car's x velo|#
         (cvy (/ (cadr (ca 'get-velo)) 2))) #|car's y celo|#
    (cond [(or (and (ptG U E) (ptG G U)) (and (ptG U G) (ptG E U)))      #|Is U inside|#
           (begin
             ((ball 'update-ball) (list (list bx (- by vy))
                                        (list (+ vx cvx) (+ (- vy) cvy)) 15))
             (list "U"))]
          [(or (and (ptG UR E) (ptG G UR)) (and (ptG UR G) (ptG E UR)))  #|Is UR inside|#
           (begin
             ((ball 'update-ball) (list (list (- bx vx) (- by vy))
                                        (list (+ (- vx) cvx) (+ (- vy) cvy)) 15))
             (list "UR"))]
          [(or (and (ptG R E) (ptG G R)) (and (ptG R G) (ptG E R)))      #|Is R inside|#
           (begin
             ((ball 'update-ball) (list (list (- bx vx) by)
                                        (list (+ (- vx) cvx) (+ vy cvy)) 15))
             (list "R"))]
          [(or (and (ptG DR E) (ptG G DR)) (and (ptG DR G) (ptG E DR)))  #|Is DR inside|#
           (begin
             ((ball 'update-ball) (list (list (- bx vx) by)
                                        (list (+ (- vx) cvx) (+ (- vy) cvy)) 15))
             (list "DR"))]
          [(or (and (ptG D E) (ptG G D)) (and (ptG D G) (ptG E D)))      #|Is D inside|#
           (begin
             ((ball 'update-ball) (list (list bx (- by vy))
                                        (list (+ vx cvx) (+ (- vy) cvy)) 15))
             (list "D"))]
          [(or (and (ptG DL E) (ptG G DL))(and (ptG DL G) (ptG E DL)))   #|Is DL inside|#
           (begin
             ((ball 'update-ball) (list (list (- bx vx) by)
                                        (list (+ (- vx) cvx) (+ (- vy) cvy)) 15))
             (list "DL"))]
          [(or (and (ptG L E) (ptG G L)) (and (ptG L G) (ptG E L)))      #|Is L inside|#
           (begin
             ((ball 'update-ball) (list (list (- bx vx) by)
                                        (list (+ (- vx) cvx) (+ vy cvy)) 15))
             (list "L"))]
          [(or (and (ptG UL E) (ptG G UL)) (and (ptG UL G) (ptG E UL)))  #|Is UL inside|#
           (begin
             ((ball 'update-ball) (list (list (- bx vx) by)
                                        (list (+ (- vx) cvx) (+ (- vy) cvy)) 15))
             (list "UL"))]
          [else (begin
                  ((ball 'update-ball) (list (list bx by)
                                             (list vx vy) 15))
                  (list hb))])))

#|
update = updates positions and velocities

@param lst = list of everything
@return    = updated everything
|#

(define (update w)
  (begin
    (world-state 'tic)
    (update-car car1)
    (update-car car2)
    (ballPos (ball 'get-pos) (ball 'get-velo) 15)
    (game-over-state 'check-for-game-over)
    w))
