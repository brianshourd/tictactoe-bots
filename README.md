tictactoe-bots
==============

This is an experiment to practice with genetic algorithms and bot
building. The program uses a typical genetic algorithm to engineer a bot
to play a game of Tic-Tac-Toe.

However, there is one interesting facet. I challenged myself to make the
bots as stupid as possible. That is, I didn't want to say things like
'automatically block a win' or 'automatically take a win' - basically, I
didn't want my own ideas of what makes a good/bad bot to influence the
algorithm at all. In other words, I wanted to see how well the algorithm
worked with a large space and a weak heuristic.

This means that a bot is literally a collection of pairs - for every
possible board state, it has knows which move it would make.

Well, there are an awful lot of board states, so each bot is actually a
terrifically huge amount of data. In order to compress this down, a list
of all possible board states is created up front, removing board states
that are just reflections or rotations of other board states. Each board
state is then packaged up with all of the possible moves remaining
(which is less than 9), and so for each board state a bot must store
only a single number smaller than 9. This can be packaged up into a
collection of bits (very carfully), so that, in the end, each bot is
only 75 16-bit words.

The bots are graded by facing off in tournaments, with the best
proceeding on, and 'breeding' with other bots by retaining move choices
from each parent, with a small amount of randomness.
