Racket League

Thaddeus Ciras

April 30, 2017


Overview
Our project was designed to ba a Rocket League - Air Hockey hybrid using racket code.

My main contribution to the code was:
 * The original physics engine (got scrapped for the one Dave wrote)
 * The ball physics and car and walls
 * Goal Scoring


My Code
So I did not end up using any libraries for this project.  I thought that I would need to use the math library, but it turns out all the funcationality I would need was already in the base racket language.

Key Code Excerpts

Most of my code is what you would call "ugly" or "spagetti" code. The code I need to write was almost entirely just conditional comparisons.  I did use some procedural abstraction, but the main material I used from this class was let and let*.

The piece of code that most exemplifies my work is the carHitBox function.  
```racket
#|
carHitBox = Calculates the hitbox for a car

@param veh = car object
@return    = list of corners of car

(a,b)      (c,d)
  +----------+
  |          |
  +----------+
(g,h)      (e,f)
|#

(define (carHitBox veh)
  (let* ((x (veh 'get-x)) #|car's x coordinate|#
         (y (veh 'get-y))  #|car's y coordinate|#
         (t (degrees->radians (veh 'get-theta)))  #|car's theta|#
         (w2 (/ car-width 2))   #|half the car's width|#
         (L2 (/ car-length 2))  #|half the car's length|#
         (ac (cos (- t pi)))    #|adjacent side length of triangle to edge|#
         (as (sin (- t pi))))   #|opposite side length of triangel to edge|#
    (list (list (+ x (* w2 ac) (- (* L2 as)))
                (+ y (* w2 as) (+ (* L2 ac))))  #|(a,b)|#
          (list (- x (* w2 ac) (- (* L2 as)))
                (+ y (* w2 as) (+ (* L2 ac))))  #|(c,d)|#
          (list (- x (* w2 ac) (- (* L2 as)))
                (- y (* w2 as) (+ (* L2 ac))))  #|(e,f)|#
          (list (+ x (* w2 ac) (- (* L2 as)))
                (- y (* w2 as) (+ (* L2 ac))))) #|(g,h)|#
    ))
```
In this code you can see the use of let*.  In order to define the corners of a rectangle from its center, two things need to be known.  You need to know the rectangle's length and width and it's rotation angle. Knowing thse things and a little trig allowed me to calculate the corners of the rectangle.  This allowed me to write the beautifuly ugly code that is my favorite part of this project, although it doesn't work properly.  It's probobly my favorite and my least favorite for that reason.
```racket
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
```
Yes, I know this is ugly, but here me out.  In order to determine if a ball is coliding with a car, the points on the cercumfrence would need to hit the edges of the rectangle.  In order to determine the edges of the rectangle you just need to know where the corners are.  It is also very difficult to determine the engire circumfrence of the ball and keep track of every point on the edge.  For that reason I defined eight points on the circunfrence of the ball.  By comparing these points to eachother, it can be determined if the ball is "inside" the car, meaning that it has colided.  Based on which point is "inside" the car, the velocity is reversed, causing the ball to appear to bounce off the car.

Improvements
My main improvement would be to get the ball to always bounce off the cars.  One of the most interesting thigs about the car not correctly coliding with the ball is that if the car is stationary, it almost always bounes correctly.  Fir this reason I believe that the issue mostly comes from two points being inside the ball at the same time messingwith the correct velocity changes, but I don't actually know if that is the case. 
