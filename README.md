# Racket League

### Statement
Our project is inspired by one of our favorite games, Rocket League. If you are unfamiliar with Rocket League, it is a video game with which you take control of cars and play soccer. You have boost which enables you to go faster, you can jump, fly through the air, and climb up walls. 

The point of this project is to create a 2-D version of the game using Racket. Seeing as though it is in two dimensions and not three, some of the functionality of the original will not be present (most notably the flying, driving up walls, etc... as we are planning on implementing this with a top down view). It will be most similar to air hockey in the way it plays.

We are interested in creating this project mostly because none of us have ever really created a game before. We've always wanted to mess around with the parts that make up a game (graphics, sound, physics, etc...) so we thought this ould be the perfect oppurtunity to do so. We hope to learn the challenges of creating a game and working on a large project as a team of developers. 

### Analysis

    * Data Abstraction -> Many of the objects depend upon an X Y coordinate and thus need various accessors to get those coordinates. The car objects, ball object, and boost objects are all abstracted in some way to make manipulating them easier.
    * Object-Orientation -> We will need a Player and Ball objects to save state data for accessing within our engine loop.
    * State-Modification -> Modified the state of objects so as not to create new ones constantly throughout the runtime of the program. Also used state modification to hold the game state to make it easier to transition between menu state and game state.
    * Map/Filter/Reduce -> Wrote custom filter procedure within the sound engine along with mapping the list of entites within the draw procedure.

### External Technologies

- Sound -> Any good game needs sound effects, so we created our own.
- Control -> Player movement will depend upon keyboard input and the whole system will depend upon a physics based engine

### Deliverable and Demonstration

  This project is more similar to air hockey than it is to rocket league. Players are able to control two cars using either the arrow keys or the wasd keys. They can drive around and interact with the ball to try to hit it into the opposing goal. The players can collide with eachother, the wall, and the ball. There is also a timer as each match only lasts two minutes. There is a scoreboard so players can see their score as well. Currently, boost is not implemented.
 
### Evaluation of Results

To be successful, we should be able to play a full match, from start to finish, without any major hiccups.

## Architecture Diagram
![Architecture Diagram](/Plan/ArchitectureDiagram.png?raw=false "Architecture Diagram")

The engine is the main driver of our game. It will keep track of the objects we have, update them when necessary, check for input from our keyboard listener, tell our sound engine when to play music, and tell our visual handler when things need to be drawn and redrawn. It also will be the area of code where the state of our objects is kept and modified. This is so that only the engine will be touching it and nothing else, hopefully reducing the amount of bugs we have.

The visual handler draws all the updated objects to the screen. It will contain a few procedures which will have our objects passed to it, which will in turn, update the state of our canvas that we are drawing on.

The sound engine continuously plays music and will play sound effects when various events happen (demolitions, scoring, etc.).

We are currently planning on using htdp/universe and their image library along with the racket math library, and the rsound library to implement our project.

## Schedule

### First Milestone (Sun Apr 9)
We have a simple menu. Player and ball classes are implemented. Both the player and ball can get drawn to the screen. A simple game loop powered by the physics engine exists.

### Second Milestone (Sun Apr 16)
We have a full menu that can be updated. Two players can get drawn to the screen and drive around. They can hit the ball and each other. If the ball goes into the goal, it disappears and a goal is scored internally on a scoreboard. 

### Public Presentation (Fri Apr 28)
Within the game itself, there is a visual scoreboard and timer to keep track of everything. The Menu allows a user to select from two different maps with varying friction levels (which should affect player and ball movement). A full 2-minute timed match can be played out.

### Did we meet our Milestones?
For the most part, we were pretty behind on our milestones. This was mostly due to the overcomplication of the physics engine. We really couldn't test our game without it working and it took much longer than inteded to get it working. The rest of the game logic was present though, so although we were behind on our milestones, I think we were successfully able to catch up and produce a working game.

## Group Responsibilities

### Alexander Infantino @infantinoalex
Team Lead
Implemented:
  * Key listener
      * Is able to handle on-key events and on-release events which drive the car and menu navigation
  * Sound Engine
      * Able to play sounds using a pre-loaded list of r-shounds that were recorded by myself
  * Menu
      * Got a basic menu working with a Start and Exit option that either start the game of show an exit background
  * Game Over Screen
      * Using Dave's World-State, I was able to show a game over screen when the timer reaches 0
  * File Hierarchy
      * Updated the file structure to move code into grouped folders for better readability.

### Thaddeus Ciras @TCiras
Implemented:
 * Physics
   * Ball - Ball bounces off the walls and has interesting interactions with the cars
   * Cars - Cars move around (although Dave fixed the issues I created) and they cannot leave the playing window
 * Goals - Goals are scored and recorded to the correct team

### Dave Caizzi @caizzi 
Implemented:
   * Graphics
      * Created a visual handler which draws all game entities to the screen.
      * Created (very basic) images for the cars.
   * Classes
      * Created Car and Ball objects which allow user to interface with and get info from.
   * Physics
      * Made the cars able to drive around the screen. Thad dealt with collision detection, though.
   * World State
      * Implemented a world state class that contained important info about the game.
      * Contained a scoreboard that tallied up each players score. Scoreboard is also drawn to screen.
      * Also features a timer which counts down from 2:00 for the game time. Timer is drawn to screen.
