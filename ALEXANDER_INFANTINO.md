# Racket League

## Alexander Infantino
### April 28, 2017

# Overview
The point of the project was to recreate the game Rocket League in 2-D space using Racket.

The code that I worked on included the Keylistener, the menu objects, menu state, game over state, and the SoundEngine.

These pieces of code ensure that Racket League actually functions as a game and is able to be replayed without draining too many resources.

# My Code

## Sound Engine

### Libraries Used

The code uses one library

```
(require rsound)
```

* The RSound library provides functionality enabling me to play various sounds and stop various sounds that were loaded from external files.

### Key Code Excerpts

Overall, I am very proud of my sound engine. I think I was able to use recursion to find something in a list, along with a global object to be referenced elsewhere to play the sounds.

#### Recursion

Before I talk about the procedures that I created in the sound engine, I want to talk about the way I represented the data I wanted in the code. When you want to play an external sound using RSound, you must call the procedure r-read.
I beleve this code actually loads up the .wav file in question and creates an RSound object from it. I didn't want it to have to load the object every single time, so I decided to create a list to store all the possible
RSound objects that I would want to use. I then thought, how would I actually retrieve these objects? I decided that creating a pair with a symbol for the RSound and the actual RSound object would work. This code can be seen below.

```
(define (sound-effects)
  (list
   (list 'goal-scored (rs-read (build-path (current-directory) "Resources" "Sound" "goal.wav")))
   (list 'explosion (rs-read (build-path (current-directory) "Resources" "Sound" "explosion.wav")))
   (list 'accelerate (rs-read (build-path (current-directory) "Resources" "Sound" "acceleration.wav")))
   (list 'blue-scored (rs-read (build-path (current-directory) "Resources" "Sound" "blue-player-scored.wav")))
   (list 'orange-scored (rs-read (build-path (current-directory) "Resources" "Sound" "orange-player-scored.wav")))
   (list 'start-game (rs-read (build-path (current-directory) "Resources" "Sound" "start-game.wav")))
   (list 'stop-game (rs-read (build-path (current-directory) "Resources" "Sound" "stop-game.wav")))
   (list 'small-collision (rs-read (build-path (current-directory) "Resources" "Sound" "small-collision.wav")))))

(define (music-effects)
  (list
   (list 'menu-music (rs-read (build-path (current-directory) "Resources" "Sound" "menu_music.wav")))))
```

Now comes the issue of how will I actually extract the RSound object? I decided to use the code below.

```
;; Finds the effects name in a given list based upon a symbol
(define (find-effect effects-name effects-list)
  (effects-list-iter effects-name (car effects-list) (cdr effects-list)))

;; Iterates through a list to find effects name
(define (effects-list-iter effects-name current-item rest-of-list)
  (cond ((eq? (car current-item) effects-name) (cadr current-item))
        ((null? rest-of-list) (error "Could Not Find Effect"))
        (else (effects-list-iter effects-name (car rest-of-list) (cdr rest-of-list)))))
```

This is my favorite code that I wrote for this project. The procedure ```find-effect``` takes in two arguments, the effects-name you want to find and the effects-list you want to search through. What it does is it recursively goes through
the list of sound-effects passed into it. It checks the car of the current-item in the list to see if it matches the effects-name you want. The car of the current-item will be a symbol in the pair of symbol rsound objects.
If it is a match, the procedure returns the cadr of the item which should be the rsound objet. If not, it recursively calls itself by getting the rest of the items and checking again. If it cannot find anything, it throws an error.

I think this code exemplifies how to both abstract data using the pairs of symbols and rsounds while also showing just how useful recursion can be.

## Key Listener

### Libraries Used

The code used one library.

```
(require 2htdp/universe)
```

This library provided the ability to use the on-key and the on-release events which are triggered when a key is either pressed down or released.

### Key Code Excerpts

The Keylistener depends on the state of two global objects used within the game itself. All it needs to know is whether or no the game is in the Menu State or not. This is because the code performs different actions
with the same keys depending on the state.

#### Object Orientation

Here you can see that there is an object called menu-state and game-over-state. These are global objects that are used to determine which code path the Keylistener must take.

```
;; Handles when a key event is found
(define (key-handler w ke)
  (cond ((menu-state 'ShowMenu?)
           (menu-key-listener w ke))
          ((game-over-state 'IsGameOver?) (game-over-handler w ke))
          (else (game-key-listener w ke))))

;; Handles when a key release happens
(define (release-handler w key)
  (if (menu-state 'ShowMenu?)
      (menu-release-listener w key)
      (game-release-listener w key)))
      
;; Excerpt of how to detect what key is pressed
...
    (cond ((key=? ke "left") (begin
                               ((car2 'turn-true) 'left)
                               w))
          ((key=? ke "right") (begin
                                ((car2 'turn-true) 'right)
                                w))
          ((key=? ke "down") (if (car2 'decel?)
                                 w
                                 (begin
                                   ((car2 'set-decel) #t)
                                   w)))
...
```

When evaluated the code simply checks which key has been pressed and calls other functions, such as those in the PhysicsEngine, to execute. I find this code to be an elegant integration of the objects created elsewhere in the code.
Without the data abstraction of 'ShowMenu? and 'IsGameOver? it would be a lot harder to determine what state the game was in and what code path to follow.

## Visual Handler

### Libraries Used

This code took advantage of

```
(require 2htdp/image)
```

This library provided the ability to draw images to the screen

### Key Code Excerpts

One of the main highlights of the Visual Handler that I worked on was the menu. We wanted a simple way for the user to interact with a menu to either start a game or exit the game. To do this, there needed to be some abstraction
for the menu itself.

#### Data Abstraction + Object Oriented

```
(define (create-menu)
  (let ((selection 1)
        (selection-box (rectangle 385 105 "outline" "white")))
    (define (update-selection type)
      (begin
        (cond ((eq? 'up type) (set! selection (+ selection 1)))
              ((eq? 'down type) (set! selection (- selection 1)))
              (else "Nothing to change"))
        (cond ((> 1 selection) (set! selection 0))
              ((< 0 selection) (set! selection 1)))))
    (define (dispatch message)
      (cond ((eq? message 'get-selection) selection)
            ((eq? message 'update-selection-up) (update-selection 'up))
            ((eq? message 'update-selection-down) (update-selection 'down))
            ((eq? message 'get-selection-box) selection-box)
            (else (error "Could Not Communicate With Menu"))))
    dispatch))
```

Taking inspiration from the bank account homework that we worked on, I decided to follow a similar method to enable messages to be passed to a menu. The point of the menu is to determine which selection
the user wants, and execute some procedure based on that selection.

First I needed an image for the selection box. I decided to use ```let (selection-box (rectangle 385 105 "outline" "white")))``` for this as it would enable the selection-box to stay within the menu itself so when I wanted to draw
the selection box, I simply needed to call ```'get-selection```. I also have an internal state within the menu ```(selection 1)```. This indicates what the user is hovering over. 1 is 'PlayGame' and 0 is 'ExitGame'. I thought it would
be easiest to represent this using integers. There are also various accessors and setters to change the state of the selection box to either 1 or 0 depending on which key was pressed.

Another piece of code that uses Data Abstraction and Object Orientation that I wanted to highlight is the game-over-state.
Dave was able to write a world-state object to keep track internally of the score and time in the game. The problem was, when the timer reached 0, it went into the negatives. I decided to create an object to handle that case
and make a game-over-state to tell the draw fucntion to draw something else.

```
(define (create-game-over)
  (let ((shouldShow #f))
    (define (check-for-game-over)
      (if (and (not (menu-state 'ShowMenu?))
               (>= 0 (- (world-state 'get-game-time) (tics->seconds (world-state 'get-tics)))))
          (set! shouldShow #t)
          "Nothing to do"))
    (define (dispatch message)
      (cond ((eq? message 'IsGameOver?) shouldShow)
            ((eq? message 'check-for-game-over) (if (not (menu-state 'ShowMenu?))
                                                    (check-for-game-over)
                                                    "Nothing to do"))
            ((eq? message 'ResetGameOver) (set! shouldShow #f))
            (else "Nothing to do")))
    dispatch))
```

This works, once again, similar to the bank account problem done in the homework for this class. I created a 'IsGameOver? message that grabs the timer from the world state and checks to see if the timer has reached 0. If it
has it sets its internal shouldShow bool to true which is accessed in the draw function telling it to show the game over screen and not the game screen. This made it really easy and simple to check to see if the game had ended.

To see how both the menu drawing and game over drawing work, you have to take a look at the draw-entites procedure that was both edited by myself and Dave.

```
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
```

As you can see, this procedure uses the Data Abstraction to see if 'ShowMenu? is true, which then does a few other checks to see what it should draw. It the menu isn't being shown it uses the game-over-state object and the data abstraction
to see if 'IsGameOver? is true. If it is it draws the game over image to the screen.

## Improvements

One major thing we could improve on is actually passing the global objects in the calls instead of just accessing them all willy nilly. The reason we did this is that using 2htdp/universe, we were having issues
create some world and storing the objects in there. Most of the procedures we had, especially the rsound ones, were causing issues when returning the world. When the rsound was called, the big-bang procedure kept
interpreting the "sound played" being return from the ```(play 'blah)``` call as the new world. This was giving us a headache so we decided for the time being to just access the objects globally. This is a big no no
because these objects should really be passed into each procedure call that requires them to be modified.

We are hoping to use a list of the entities (since racket isn't strongly typed like haskell we can have a list of different objects in there) and pass that to the calls and using ```car``` and such, actually iterating through the list
using either fold or map to actually change the state of these objects.

## List of Code Worked On by Me

I just wanted to list the major pieces of code I worked on

* Everything in soundengine.rkt
* Everything in soundsources.rkt
* The majority of the keylistener.rkt
	* A few minor tweeks were done by Dave and Thad to help for readability.
* VisualHandler.rkt
		* draw-menu
		* create-menu-state
		* create-menu
		* create-game-over
		* create-menu
		* reset

		
