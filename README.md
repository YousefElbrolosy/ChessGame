# ChessGame
In this project a chess game is implemented in Haskell

For simplicity, we are assuming the following:
a) There are no promotions of pawns.
b) It is legal for a king to move to locations in which he will be threatened.

The following functions are implemented:

setBoard :: Board
The function does not take any inputs and returns a board representing the initial
configuration. Assume that the first turn is always on the
white player.

visualizeBoard:: Board->String
The function takes as input a board and returns a visual representation of the board in a string. Black pieces are suffixed
with ’B’ and white pieces are suffixed with ’W’.

isLegal:: Piece -> Board -> Location -> Bool
The function takes as input a piece, a board, and a location. It returns True if
the move of the piece on the given board to the input location is legal, and False
otherwise.

suggestMove:: Piece -> Board -> [Location]
The function takes as input a piece and a board and outputs a list of possible legal
next locations for the piece.

move:: Piece -> Location -> Board -> Board
The function takes as input a
piece, a location, and a board and returns a new updated board after the move is
applied if it is a legal move. Otherwise, if the move is illegal, the function throws
an appropriate error
