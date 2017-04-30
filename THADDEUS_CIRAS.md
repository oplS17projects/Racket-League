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
```
Put the code here
```
In this code you can see the use of let*.  In order to define the corners of a rectangle from its center, two things need to be known.  You need to know the rectangle's length and width and it's rotation angle. Knowing thse things and a little trig allowed me to calculate the corners of the rectangle.  This allowed me to write the beautifuly ugly code that is my favorite part of this project, although it doesn't work properly.  It's probobly my favorite and my least favorite for that reason.
```
Put the code here
```
Yes, I know this is ugly, but here me out.  In order to determine if a ball is coliding with a car, the points on the cercumfrence would need to hit the edges of the rectangle.  In order to determine the edges of the rectangle you just need to know where the corners are.  It is also very difficult to determine the engire circumfrence of the ball and keep track of every point on the edge.  For that reason I defined eight points on the circunfrence of the ball.  By comparing these points to eachother, it can be determined if the ball is "inside" the car, meaning that it has colided.  Based on which point is "inside" the car, the velocity is reversed, causing the ball to appear to bounce off the car.

Improvements
My main improvement would be to get the ball to always bounce off the cars.  One of the most interesting thigs about the car not correctly coliding with the ball is that if the car is stationary, it almost always bounes correctly.  Fir this reason I believe that the issue mostly comes from two points being inside the ball at the same time messingwith the correct velocity changes, but I don't actually know if that is the case. 
