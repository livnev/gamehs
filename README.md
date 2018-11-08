# some game abstractions

`Game.hs` defines the `Game` typeclass, and methods sufficient to simulate gameplay. `Controller.hs` implements some controllers for this typeclass, for example `monte1000Controller` which use Monte Carlo Tree Search to implement a player for any `Game`. `Tic9` implements ordinary tic-tac-toe and `Tic81` implements [meta tic-tac-toe](https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe).
