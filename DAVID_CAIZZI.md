# Racket League

## David Caizzi
### April 30, 2017

# Overview
The goal of this project was to create a 2D version of the game Rocket League using Racket. Rocket League is a driving/soccer game where players cars to knock balls into goals.
Our version, Racket-League, is simplified but still features homemade graphics and sounds, complex physics, and lots of fun.

I had a variety of things I worked on in the code, but the main things I contributed were:
* The creation of the classes that the game uses
* Drawing all the action to the screen
* Enabling the cars to drive

**Authorship note:** The majority of the code described here was written by myself. All other contributions are noted.

# Libraries Used
The code that I wrote relies on two main libraries.
``` racket 
(require 2htdp/universe)
(require 2htdp/image)
```
* The universe library provided the framework for the main game loop and drawing functions.
* The image library provided the ability to make the images (cars, ball, boost, etc.) that would be drawn to the screen.

# Key code excerpts
Listed are my contributions to the code that i'm most proud of/are the best embodiment of ideas from our OPL class.

## 1. Drawing entities featuring recursion and accumulation
When trying to figure out a way to draw everything to the screen, recursion made the most sense to me. 
I had a list of entities that all needed to be drawn to the screen so I created a recursive draw function that walked down a list and drew everything.

``` racket
(define (draw-entities t)
    (if (menu-state 'ShowMenu?)
        (if (menu-state 'ShouldExit?)
            exit-background
            (draw-menu menu-background))
        (if (game-over-state 'IsGameOver?)
            game-over-background
            (rhelp entities (place-image (world-state 'get-scoreboard)
                                         500
                                         30                                     
                                         (place-image (world-state 'get-timer)
                                                      500
                                                      720
                                                      background))))))


(define (rhelp lst scene)
  (if (null? lst)
      scene
      (rhelp (cdr lst)
             (if ((car lst) 'active?)
                 (place-image  ((car lst) 'get-image)
                               ((car lst) 'get-x)
                               ((car lst) 'get-y)
                               scene)
                 scene))))
                 
```

I wrote the ```rhelp``` procedure to accomplish this task (all the menu stuff was Alex). 
The procdure adds every entity to its corresponding location in a scene which is what's eventually drawn to the screen. 
A background image is the initial state of the scene and it features the field and the current state of the tier and scoreboard.

After thinking about my ```rhelp``` procedure a little bit, I came to the conclusion that all it is is an accumulation or a fold where the final product is the ending scene.
When I realized this, I decided to implement the proceudre as a ```foldl```.
``` racket
(define (draw-entities t)
    (if (menu-state 'ShowMenu?)
        (if (menu-state 'ShouldExit?)
            exit-background
            (draw-menu menu-background))
        (if (game-over-state 'IsGameOver?)
            game-over-background
            (foldl
             (lambda (entity scene)
               (if (entity 'active?)
                   (place-image (entity 'get-image)
                                (entity 'get-x)
                                (entity 'get-y)
                                scene)
                   scene))
             (place-image (world-state 'get-scoreboard)
                                         500
                                         30                                     
                                         (place-image (world-state 'get-timer)
                                                      500
                                                      720
                                                      background))
             
               entities)))
```

This is pretty much the same as ```rhelp``` except it uses a ```foldl``` where the initial value is the background scene, the list is the entities, and the procedure adds each eneitty into the scene.


## 2. Classes featuring object orientation and message passing
I'd say that my biggest contribution to the project was my creation of all the classes that the games used.
These classes served as the backbone for the game to function.

I wrote many classes for the game including car, ball, boost powerup, and world-state classes, however, I've only included the world-state class as it showcases many important concepts and to keep things short and sweet.
``` racket
(define (make-game)
  (let ((tics 0)
        (score '(0 0))
        (game-time 120)) ; Game total time in seconds
    (define (reset-game)
      (begin
        (set! tics 0)
        (set! score '(0 0))
        (set! game-time 120)))
    (define (dispatch m)
      (cond ((eq? m 'get-tics) tics)
            ((eq? m 'tic) (set! tics (add1 tics)))
            ((eq? m 'get-score) score)
            ((eq? m 'left-score) (begin
                                   ((sound-engine 'play-sound-effect) 'blue-scored)
                                   (set! score (list (add1 (car score)) (cadr score)))))
            ((eq? m 'right-score) (begin
                                    ((sound-engine 'play-sound-effect) 'orange-scored)
                                    (set! score (list (car score) (add1 (cadr score))))))
            ((eq? m 'get-game-time) game-time)
            ((eq? m 'get-timer) (text/font
                                (seconds->timer (- game-time (tics->seconds tics)))
                                60
                                "red"
                                #f
                                'modern
                                'normal
                                'bold
                                #f))
            ((eq? m 'get-scoreboard) (text/font
                                     (string-append (number->string (car score)) "      " (number->string (cadr score)))
                                     60
                                     "red"
                                     #f
                                     'modern
                                     'normal
                                     'bold
                                     #f))
            ((eq? m 'reset-game) (reset-game))
            (else "Invalid message passed to game object.")))
    dispatch))
```
This ```make-game``` procedure creates an enclousre that stores a lot of important information about the game. 
Using ```'get-score``` and ```'get-game-time``` returns the games current score and the current time left in the game, as you'd expect.
```'get-timer``` and ```'get-scoreboard``` return the timer and scoreboard in image format so they can be easily displayed to the screen.
Messages like ```'tic``` and ```'left-score``` allow the state of the world to be changed.

```make-game``` highlights how important object-orientation and message passing are to this project.
Unfortunately, it's nearly impossible to create a game like this without modifying state data.
We avoided using ```set!``` throughout the code and abstracted all of our state modification behind closures.

Other cool things about this proceudre is it features some procedural abstraction in the form of ```tics->seconds``` and ```seconds->timer```
```racket
;Converts a number of seconds to a timer with minutes and seconds

(define (seconds->timer s)
  (let* ((mins (quotient s 60))
        (secs (remainder s 60))
        (min-string (number->string mins))
        (sec-string (number->string secs)))
    (string-append
     min-string
     ":"
     (if (= (string-length sec-string) 1)
         (string-append "0" sec-string)
         sec-string))))

; Converts an amount of clock tics to a number of seconds

(define (tics->seconds tic)
  (quotient tic 30))
```
These procedures weren't really used much but they were helpful in keeping my code somewhat clean and readable.
They do pretty much what you would expect them to. ```tics->seconds``` converts game-clock-cycles into seconds and ```seconds->timer``` converts an amount of seconds into a string in the format of a timer.

## 3. Finding new velocity featuring let\*
The last piece of code I want to showcase is the one I'm the most proud of. 
This code is the procedure used for speeding a car up or down, depending on whether or not it's accelerating.

``` racket
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
```

```accelerate-car``` takes a car object, finds its new velocity, and updates the car with the new velocity via message-passing and procedure application.
First, it uses a ```let*``` to calculate some important info about the car such as its angle and x and y-components of its velocity.
It then uses message-passing to check if the car is currently accelerating or reversing and adjusts the cars velocity accordingly.
If the car is neither accelerating nor reversing, it uses the ```slow-car``` procedure to apply drag to the car.

The reason I'm most proud of this procedure is because getting the cars to drive was the most complicated thing I did and ```accelerate-car``` was the most crucial procedure in this process.
As a group, it took us far too long to be able to get the cars to drive so getting this procedure (as well as others needed for driving) to work was a huge relief.
