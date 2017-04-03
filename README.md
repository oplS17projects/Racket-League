# Racket League

### Statement
Our project is inspired by one of our favorite games, Rocket League. If you are unfamiliar with Rocket League, it is a video game with which you take control of cars and play soccer. You have boost which enables you to go faster, you can jump, fly through the air, and climb up walls. 

The point of this project is to create a 2-D version of the game using Racket. Seeing as though it is in two dimensions and not three, some of the functionality of the original will not be present (most notably the flying, driving up walls, etc... as we are planning on implementing this with a top down view). It will be most similar to air hockey in the way it plays.

We are interested in creating this project mostly because none of us have ever really created a game before. We've always wanted to mess around with the parts that make up a game (graphics, sound, physics, etc...) so we thought this ould be the perfect oppurtunity to do so. We hope to learn the challenges of creating a game and working on a large project as a team of developers. 

### Analysis

    * Data Abstraction -> Many of the objects will be using will depend upon an X Y coordinate and will need various accessors to get those coordinates.
    * Object-Orientation -> We will need a Player and Ball objects to save state data for accessing within our engine loop.
    * State-Modification -> Will apply to our objects that we create since we will need to constantly redraw images and change the state of the objects based on what is happening in the game. State-Modification will help us manage our memory.
    * Map/Filter/Reduce -> We are still trying to design the finer details of our code, but we do expect to use these to help us manage our objects.

### External Technologies
You are encouraged to develop a project that connects to external systems. For example, this includes systems that:

- Sound -> Any good game needs sound effects, so we are planning on creating our own.
- Control -> Player movement will depend upon keyboard input and the whole system will depend upon a physics based engine

### Deliverable and Demonstration

  The plan for this project is to have an air-hockey like game where that allows two players to play at the same time against eachother on the same computer.  The cars should always go forward in the direction they are facing and the ball should bounce off surfaces at the correct angles.
  There will also be different field designs and a GUI allowing for the alteration of the ball's size, elastiticy, etc.  
  If all goes exteramly well, we will implement basic AIs.  One would simply chase the ball, while the other would play goal.  
 
### Evaluation of Results
How will you know if you are successful? 
If you include some kind of _quantitative analysis,_ that would be good.

To be successful, we should be able to play a full match, from start to finish, without any hiccups. All of our physics procedures should be unit tested along with our basic game logic.

## Architecture Diagram
![Architecture Diagram](/ArchitectureDiagram.png?raw=false "Architecture Diagram")

The engine gets user input from the keyboard listener. Based off this user input, the various objects are updated and potential collisions are dealt with.

The visual handler draws all the updated objects to the screen.

The sound engine continuously plays music and will play sound effects when various events happen (demolitions, scoring, etc.).

## Schedule

### First Milestone (Sun Apr 9)
We have a simple menu. Player and ball classes are implemented. Both the player and ball can get drawn to the screen. A simple game loop powered by the physics engine exists.

### Second Milestone (Sun Apr 16)
We have a full menu that can be updated. Two players can get drawn to the screen and drive around. They can hit the ball and each other. If the ball goes into the goal, it disappears and a goal is scored internally on a scoreboard. 
### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
Within the game itself, there is a visual scoreboard and timer to keep track of everything. The Menu allows a user to select from two different maps with varying friction levels (which should affect player and ball movement). A full 2-minute timed match can be played out.

## Group Responsibilities

### Alexander Infantino @infantinoalex
Team Lead
Alex will be working on the menu, sound effects, and unit testing.

### Thaddeus Ciras @TCiras
Thad will be working on...
 * Physics
   * Ball
   * Players
 * Boost Calculations
 * Demolition
 * AI - Goalie

### Dave Caizzi @caizzi 
Dave will be working on
* Creation of classes
* Drawing images to the screen
