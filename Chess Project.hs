import Data.Char (ord, chr)
type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])


setBoard :: Board
setBoard = (White, 
	[R ('h',1),N ('g',1), B ('f',1), Q ('e',1), K ('d',1), B ('c',1), N ('b',1), R ('a',1),
	 P ('h',2),P ('g',2), P ('f',2), P ('e',2), P ('d',2), P ('c',2), P ('b',2), P ('a',2)],
	[R ('h',8),N ('g',8), B ('f',8), K ('e',8), Q ('d',8), B ('c',8), N ('b',8), R ('a',8),
	 P ('h',7),P ('g',7), P ('f',7), P ('e',7), P ('d',7), P ('c',7), P ('b',7), P ('a',7)])

-- If the loc I pass has N in the white then NW else if it is in the black  then NB and so on for all pieces  
	   
isLegal :: Piece -> Board -> Location -> Bool
  {-
isLegal (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
	|x=="P"= isLegalP (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
	|x=="Q"= isLegalQ (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
	|x=="N"= isLegalN (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
	|x=="B"= isLegalB (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
	|x=="K"= isLegalK (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
	|x=="R"= isLegalR (x (col, row)) (color, whitePieces, blackPieces) (atcCol, atcRow)
-}
-- isLegal

isLegal (P (col, row)) (Black, whitePieces, blackPieces) (atcCol, atcRow)
   | row == 2 && col == atcCol && (atcRow == row + 1 || atcRow == row + 2) && getPiece (atcCol, row + 1) whitePieces blackPieces == "" = True
   | atcRow == row + 1 && atcCol==col && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = True
   | atcRow == row + 1 && ord(atcCol)==ord(col)+1 && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | atcRow == row + 1 && ord(atcCol)==ord(col)-1 && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | otherwise = False
-- Pawn
isLegal (P (col, row)) (White, whitePieces, blackPieces) (atcCol, atcRow)
   | row == 2 && col == atcCol && (atcRow == row + 1 || atcRow == row + 2) && getPiece (atcCol, row + 1) whitePieces blackPieces == "" = True
   | atcRow == row + 1 && atcCol==col && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = True
   | atcRow == row + 1 && ord(atcCol)==ord(col)+1 && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
	| atcRow == row + 1 && ord(atcCol)==ord(col)-1 && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | otherwise = False
-- Knight

isLegal (N (col, row)) (Black, whitePieces, blackPieces) (atcCol, atcRow)
   | row+1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row-1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | ord(atcCol)==ord(col)+2 && (row+1 == atcRow || row-1 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True 
   | ord(atcCol)==ord(col)-2 && (row+1 == atcRow || row-1 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   
   | ord(atcCol)==ord(col)+1 && (row+2 == atcRow || row-2 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True 
   | ord(atcCol)==ord(col)-1 && (row+2 == atcRow || row-2 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   
   | ord(atcCol)==ord(col)+1 && (row+2 == atcRow || row-2 == atcRow ) &&  elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True 
   | ord(atcCol)==ord(col)-1 && (row+2 == atcRow || row-2 == atcRow ) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row+1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | row-1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | ord(atcCol)==ord(col)+2 && (row+1 == atcRow || row-1 == atcRow ) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | ord(atcCol)==ord(col)-2 && (row+1 == atcRow || row-1 == atcRow ) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | otherwise = False
 
 
isLegal (N (col, row)) (White, whitePieces, blackPieces) (atcCol, atcRow)
   | row+1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row-1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row-1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | ord(atcCol)==ord(col)+2 && (row+1 == atcRow || row-1 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True 
   | ord(atcCol)==ord(col)-2 && (row+1 == atcRow || row-1 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   
   | ord(atcCol)==ord(col)+1 && (row+2 == atcRow || row-2 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True 
   | ord(atcCol)==ord(col)-1 && (row+2 == atcRow || row-2 == atcRow ) && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   
   | ord(atcCol)==ord(col)+1 && (row+2 == atcRow || row-2 == atcRow ) &&  elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True 
   | ord(atcCol)==ord(col)-1 && (row+2 == atcRow || row-2 == atcRow ) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row+1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | row-1 == atcRow &&   (ord(atcCol)==ord(col)+2 || ord(atcCol)==ord(col)-2) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | ord(atcCol)==ord(col)+2 && (row+1 == atcRow || row-1 == atcRow ) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | ord(atcCol)==ord(col)-2 && (row+1 == atcRow || row-1 == atcRow ) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  = True 
   | otherwise = False 

--Rook

isLegal (R (col, row)) (Black, whitePieces, blackPieces) (atcCol, atcRow)
   | col == atcCol && row<atcRow  && checkFree (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col == atcCol &&  row>atcRow  && checkFreeL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)<ord(atcCol) && checkFreeC (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)>ord(atcCol) && checkFreeCL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col == atcCol && row<atcRow && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | col == atcCol &&  row>atcRow && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row == atcRow && ord(col)<ord(atcCol) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | row == atcRow && ord(col)>ord(atcCol) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)   =True
   | otherwise = False

isLegal (R (col, row)) (White, whitePieces, blackPieces) (atcCol, atcRow)
   | col == atcCol && row<atcRow  && checkFree (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col == atcCol &&  row>atcRow  && checkFreeL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)<ord(atcCol) && checkFreeC (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)>ord(atcCol) && checkFreeCL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col == atcCol && row<atcRow && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)    =True
   | col == atcCol &&  row>atcRow && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)   =True
   | row == atcRow && ord(col)<ord(atcCol) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | row == atcRow && ord(col)>ord(atcCol) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | otherwise = False
   
   

 
--bishop
isLegal (B (col, row)) (Black, whitePieces, blackPieces) (atcCol, atcRow)   
   | col > atcCol && row>atcRow  && checkFreeDL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col < atcCol &&  row>atcRow  && checkFreeDR (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)>ord(atcCol) && checkFreeUL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)<ord(atcCol) && checkFreeUR (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col > atcCol && row>atcRow && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)   =True
   | col < atcCol &&  row>atcRow && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | row < atcRow && ord(col)>ord(atcCol) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row < atcRow && ord(col)<ord(atcCol) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | otherwise = False
  
isLegal (B (col, row)) (White, whitePieces, blackPieces) (atcCol, atcRow)   
   | col > atcCol && row>atcRow  && checkFreeDL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col < atcCol &&  row>atcRow  && checkFreeDR (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)>ord(atcCol) && checkFreeUL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)<ord(atcCol) && checkFreeUR (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col > atcCol && row>atcRow && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)   =True
   | col < atcCol &&  row>atcRow && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)   =True
   | row < atcRow && ord(col)>ord(atcCol) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | row < atcRow && ord(col)<ord(atcCol) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | otherwise = False
  
  

--king
isLegal (K (col, row)) (White, whitePieces, blackPieces) (atcCol, atcRow)   
   | chr (ord col + 1) == atcCol && row==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col - 1)== atcCol &&  row==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row -1== atcRow && col==atcCol && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row +1 == atcRow && col==atcCol && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col + 1)== atcCol && row-1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col - 1) == atcCol && row-1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col + 1) == atcCol && row+1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col - 1) == atcCol && row+1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col + 1) == atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) &&  row==atcRow   =True
   | chr (ord col - 1)== atcCol  && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)&&  row==atcRow   =True
   | row -1== atcRow && col==atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row +1 == atcRow && col==atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | chr (ord col + 1)== atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) && row-1 ==atcRow  =True
   | chr (ord col - 1) == atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)&& row-1 ==atcRow  =True
   | chr (ord col + 1) == atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)&& row+1 ==atcRow  =True
   | chr (ord col - 1) == atcCol && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)&& row+1 ==atcRow  =True
   | otherwise = False


isLegal (K (col, row)) (Black, whitePieces, blackPieces) (atcCol, atcRow)   
   | chr (ord col + 1) == atcCol && row==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col - 1)== atcCol &&  row==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row -1== atcRow && col==atcCol && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | row +1 == atcRow && col==atcCol && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col + 1)== atcCol && row-1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col - 1) == atcCol && row-1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col + 1) == atcCol && row+1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col - 1) == atcCol && row+1 ==atcRow  && (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" =True
   | chr (ord col + 1) == atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) &&  row==atcRow   =True
   | chr (ord col - 1)== atcCol  && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)&&  row==atcRow   =True
   | row -1== atcRow && col==atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row +1 == atcRow && col==atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | chr (ord col + 1)== atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) && row-1 ==atcRow  =True
   | chr (ord col - 1) == atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)&& row-1 ==atcRow  =True
   | chr (ord col + 1) == atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)&& row+1 ==atcRow  =True
   | chr (ord col - 1) == atcCol && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)&& row+1 ==atcRow  =True
   | otherwise = False


--Queen
isLegal (Q (col, row)) (Black, whitePieces, blackPieces) (atcCol, atcRow)
   | col == atcCol && row<atcRow  && checkFree (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col == atcCol &&  row>atcRow  && checkFreeL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)<ord(atcCol) && checkFreeC (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)>ord(atcCol) && checkFreeCL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col > atcCol && row>atcRow  && checkFreeDL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col < atcCol &&  row>atcRow  && checkFreeDR (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)>ord(atcCol) && checkFreeUL (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)<ord(atcCol) && checkFreeUR (col,row) (atcCol,atcRow) (Black, whitePieces, blackPieces) ==True =True
   | col == atcCol && row<atcRow && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | col == atcCol &&  row>atcRow && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | row == atcRow && ord(col)<ord(atcCol) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)  =True
   | row == atcRow && ord(col)>ord(atcCol) && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces)=True
   | col > atcCol && row>atcRow  && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | col < atcCol &&  row>atcRow  && elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row < atcRow && ord(col)>ord(atcCol)&& elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row < atcRow && ord(col)<ord(atcCol)&& elem 'W' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | otherwise = False


isLegal (Q (col, row)) (White, whitePieces, blackPieces) (atcCol, atcRow)
   | col == atcCol && row<atcRow  && checkFree (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col == atcCol &&  row>atcRow  && checkFreeL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)<ord(atcCol) && checkFreeC (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row == atcRow && ord(col)>ord(atcCol) && checkFreeCL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col > atcCol && row>atcRow  && checkFreeDL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col < atcCol &&  row>atcRow  && checkFreeDR (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)>ord(atcCol) && checkFreeUL (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | row < atcRow && ord(col)<ord(atcCol) && checkFreeUR (col,row) (atcCol,atcRow) (White, whitePieces, blackPieces) ==True =True
   | col == atcCol && row<atcRow && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | col == atcCol &&  row>atcRow && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row == atcRow && ord(col)<ord(atcCol) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row == atcRow && ord(col)>ord(atcCol) && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | col > atcCol && row>atcRow  && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)=True
   | col < atcCol &&  row>atcRow  && elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row < atcRow && ord(col)>ord(atcCol)&& elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces) =True
   | row < atcRow && ord(col)<ord(atcCol)&& elem 'B' (getPiece (atcCol, atcRow) whitePieces blackPieces)=True
   | otherwise = False
--diagonals
checkFreeUL :: Location -> Location -> Board -> Bool
checkFreeUL _ ('\0', _) (_, _, _) = True
checkFreeUL (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcCol == col && atcRow == row = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False 
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeUL (col, row) (chr (ord atcCol + 1), atcRow - 1) (x, whitePieces, blackPieces)
  | otherwise = False

checkFreeUR :: Location -> Location -> Board -> Bool
checkFreeUR _ ('\0', _) (_, _, _) = True
checkFreeUR (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcCol == col && atcRow == row = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False  
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeUR (col, row) (chr (ord atcCol - 1), atcRow - 1) (x, whitePieces, blackPieces)
  | otherwise = False

checkFreeDL :: Location -> Location -> Board -> Bool
checkFreeDL _ ('\0', _) (_, _, _) = True
checkFreeDL (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcCol == col && atcRow == row = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False  
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeDL (col, row) (chr (ord atcCol + 1), atcRow + 1) (x, whitePieces, blackPieces)
  | otherwise = False

checkFreeDR :: Location -> Location -> Board -> Bool
checkFreeDR _ ('\0', _) (_, _, _) = True
checkFreeDR (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcCol == col && atcRow == row = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False 
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeDR (col, row) (chr (ord atcCol - 1), atcRow + 1) (x, whitePieces, blackPieces)
  | otherwise = False

--up,down,left,right
checkFree :: Location -> Location -> Board -> Bool
checkFree _ (_,0) (_, _, _) = True
checkFree (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcRow == row = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False  
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFree (col, row) (atcCol, atcRow - 1) (x, whitePieces, blackPieces)
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFree (col, row) (atcCol, atcRow - 1) (x, whitePieces, blackPieces)
  | otherwise = False

checkFreeL :: Location -> Location -> Board -> Bool
checkFreeL _ (_, _) (_, _, _) = True
checkFreeL (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcRow == row = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False  
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeL (col, row) (atcCol, atcRow + 1) (x, whitePieces, blackPieces)
  | otherwise = False

checkFreeC :: Location -> Location -> Board -> Bool
checkFreeC _ ('\0', _) (_, _, _) = True
checkFreeC (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcCol == col = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False  
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeC (col, row) (chr (ord atcCol - 1), atcRow) (x, whitePieces, blackPieces)
  | otherwise = False

checkFreeCL :: Location -> Location -> Board -> Bool
checkFreeCL _ ('\0', _) (_, _, _) = True
checkFreeCL (col, row) (atcCol, atcRow) (x, whitePieces, blackPieces)
  | atcCol == col = True
  | atcRow < 1 || atcRow > 8 || atcCol < 'a' || atcCol > 'h' = False  
  | (getPiece (atcCol, atcRow) whitePieces blackPieces) == "" = checkFreeCL (col, row) (chr (ord atcCol + 1), atcRow) (x, whitePieces, blackPieces)
  | otherwise = False

main :: IO ()
main = visualizeBoard setBoard


-- part d
suggestMove:: Piece -> Board -> [Location]
suggestMove (currentplace) (p,w,b) =helpersuggestmove (currentplace) (p,w,b) ('a',8)


helpersuggestmove::Piece -> Board -> Location->[Location]
helpersuggestmove (currentplace) board (c,0)=[]
helpersuggestmove  (currentplace) board (c,i) 	   |c>'h'=helpersuggestmove (currentplace) board ('a',i-1)
									   |isLegal (currentplace) board (c,i)=(c,i):helpersuggestmove (currentplace) board (succ c,i)
									   |otherwise =helpersuggestmove (currentplace) board(succ c,i)

---(e)
isLegalTurn piece location (color,x,y)= if(((elem piece x) && (color == Black)) || ((elem piece y) && (color == White)))
																				then 
																					False
																				else 
																					True

isLegalBounds piece (a,b) = if (a>'h' || a<'a' || b>8 || b<1)
								then False
								else True	
								
replaceLocation (P(x,y)) (a,b) = P(a,b)
replaceLocation (R(x,y)) (a,b) = R(a,b)
replaceLocation (N(x,y)) (a,b) = N(a,b)
replaceLocation (B(x,y)) (a,b) = B(a,b)
replaceLocation (K(x,y)) (a,b) = K(a,b)
replaceLocation (Q(x,y)) (a,b) = Q(a,b)



replacePiece _ _ [] = []
replacePiece piece location (p:ps) = if (piece == p) then ((replaceLocation piece location):ps) else p:(replacePiece piece location ps)

removeOppPiece _ _ [] = []
removeOppPiece piece (c,d) ((P(a,b)):ps) = if ((a==c) && (b == d)) then ps else (P(a,b)):(removeOppPiece piece (c,d) ps)
removeOppPiece piece (c,d) ((R(a,b)):ps) = if ((a==c) && (b == d)) then ps else (R(a,b)):(removeOppPiece piece (c,d) ps)
removeOppPiece piece (c,d) ((N(a,b)):ps) = if ((a==c) && (b == d)) then ps else (N(a,b)):(removeOppPiece piece (c,d) ps)
removeOppPiece piece (c,d) ((B(a,b)):ps) = if ((a==c) && (b == d)) then ps else (B(a,b)):(removeOppPiece piece (c,d) ps)
removeOppPiece piece (c,d) ((K(a,b)):ps) = if ((a==c) && (b == d)) then ps else (K(a,b)):(removeOppPiece piece (c,d) ps)
removeOppPiece piece (c,d) ((Q(a,b)):ps) = if ((a==c) && (b == d)) then ps else (Q(a,b)):(removeOppPiece piece (c,d) ps)

getInt 8 = "8"
getInt 7 = "7"
getInt 6 = "6"
getInt 5 = "5"
getInt 4 = "4"
getInt 3 = "3"
getInt 2 = "2"
getInt 1 = "1"

pieceToString (P(a,b))= "P" ++" "++"("++"'"++[a]++"'"++"," ++ (getInt b)++")"
pieceToString (R(a,b))= "R" ++" "++"("++"'"++[a]++"'"++"," ++ (getInt b)++")"
pieceToString (N(a,b))= "N" ++" "++"("++"'"++[a]++"'"++"," ++ (getInt b)++")"
pieceToString (B(a,b))= "B" ++" "++"("++"'"++[a]++"'"++"," ++ (getInt b)++")"
pieceToString (K(a,b))= "K" ++" "++"("++"'"++[a]++"'"++"," ++ (getInt b)++")"
pieceToString (Q(a,b))= "Q" ++" "++"("++"'"++[a]++"'"++"," ++ (getInt b)++")"


move:: Piece -> Location -> Board -> Board 
move _ _ (_,[],[]) = error "Where is the board?"
move piece location (color,x,y) =  if((isLegal piece( color,x,y) location) && isLegalBounds piece location && isLegalTurn piece location (color,x,y)) then(
																				(if(color == White) then
																										(Black,(replacePiece piece location x),(removeOppPiece piece location y))
																									else 
																										(White,(removeOppPiece piece location x),(replacePiece piece location y))))

																			
																				else (
																				
																				if((isLegalTurn piece location (color,x,y)) == False)
																					then
																						(if ((elem piece x) && (color == Black)) then (error "This is Black player's turn, White can't move.")
																																 else (error "This is White player's turn, Black can't move."))
																					else(
																						if((isLegalBounds piece location) == False) then (error ("Move is out of bounds."))
																																					
																																	else
																																	(error ("Illegal move for piece "++(pieceToString piece)))))
