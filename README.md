# Haskell Connect Four Game
Haskell project for Emurgo Cardano Associate course

To solve some GL related issues during build, try:

<pre>sudo apt install libgl1-mesa-dev</pre>

<pre>sudo apt install freeglut3{,-dev}</pre>

To run the game use:

<pre>cabal run</pre>

Rules:

Use the keyboard arrows Left and Right to move the cursor. Use the Down arrow to speed up the movement.

The game speed can be adjusted in the Game.conf file. The minimum speed is 1.

In the current version both the Player and the Opponent are controlled by the user.

Win the game by connecting four black pieces in any direction.
