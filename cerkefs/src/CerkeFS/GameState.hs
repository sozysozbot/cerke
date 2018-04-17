module CerkeFS.GameState
(Fullboard(..)
,playFromStart
,initialBoard
--,movePieceFromTo2
--,pass
,plays
,drops
,passes
,drops'
,mun1
,plays'
,playsTam
,Dat2(..)
,declare
,taxot1
,Operation
,(>+>),(>->)
)where
import CerkeFS.Internal.Board
import CerkeFS.PrettyPrint(initialBoard)
import CerkeFS.Piece3
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.MultiSet as S

data Fullboard = Fullboard{
-- turn :: Maybe Side, -- `Nothing` means no info about the turn
 board :: Board1,
 hand :: [Piece] -- whose hand the piece is in is designated in Piece itself
} deriving(Show, Eq, Ord)

type Operation = StateT Fullboard M

(>+>), (>->) :: Monad m => (Side -> m a) -> (Side -> m b) -> m b
f >+> g = f Upward >> g Downward
f >-> g = f Downward >> g Upward

playFromStart :: Monad m => StateT Fullboard m a -> m Fullboard
playFromStart p = execStateT p Fullboard{board = initialBoard, hand = []}

liftBoardOp :: Monad m => (Board1 -> m (b, Board1)) -> StateT Fullboard m b
liftBoardOp op = do
 fb <- get
 (a, newBoard) <- lift $ op (board fb)
 put $ fb{board = newBoard}
 return a

liftBoardOpFoo :: Monad m => (Board1 -> m Board1) -> StateT Fullboard m ()
liftBoardOpFoo op = do
 fb <- get
 newBoard <- lift $ op (board fb)
 put $ fb{board = newBoard}

{-
**********
* Monadic fullboard operation
**********
-}

type Validator = Maybe PhantomPiece -> Either Error ()

-- plays under the condition
validatesPlaying, validatesTaking :: Validator -> (Square, Square, Side) -> Operation ()
validator `validatesPlaying` (from, to, sid) = do
 fb <- get
 case movePieceFromToFull from to (board fb) of
  Left (AlreadyOccupied _) -> validator `validatesTaking` (from, to, sid)
  Right (phantom, newBoard) -> case validator phantom of
   Right () -> put $ fb{board = newBoard}
   Left e -> lift $ Left e
  Left e -> lift $ Left e 


validator `validatesTaking` (from, to, sid) = do
 piece <- liftBoardOp $ removePiece to
 case getSide piece of
  Nothing -> lift $ Left TamCapture
  Just s -> if s == sid then lift $ Left FriendlyFire else do
   let Just flippedPiece = flipSide piece -- fails for Tam2, which doesn't belong here
   phantom <- liftBoardOp $ movePieceFromToFull from to
   case validator phantom of
    Right () -> modify (\fb -> fb{hand = flippedPiece : hand fb})
    Left e   -> lift $ Left e

-- | Moves the piece. If the destination is blocked, the piece at the destination is implicitly captured.
--
--   * 'MovingOpponentPiece' is raised if the side of the moving piece does not match the side given in the argument.
--
--   * 'FriendlyFire' is raised if the destination is blocked by the piece belonging to the same side as the moving piece.
--
--   * 'TamCapture' is raised if the destination is occupied by Tam2, which cannot be captured.
-- 
-- __/FIXME: Uai protection not implemented/__
plays :: Square -> Square -> Side -> Operation ()
plays from to sid = f `validatesPlaying` (from, to, sid) where
 f Nothing = return ()
 f (Just(_, _, q)) = if sid == q then return () else Left MovingOpponentPiece

-- | Similar to 'plays', but also checks if the moving piece has the profession specified by the argument.
plays' :: Square -> Profession -> Square -> Side -> Operation ()
plays' from prof to sid = f `validatesPlaying` (from, to, sid) where
 f Nothing = Left WrongProfessionSpecified{expected = Nothing, specified = Just prof}
 f (Just(_, p, q)) = case(sid == q, prof == p) of
  (False, _) -> Left MovingOpponentPiece -- side violation is louder than wrong profession
  (True, False) -> Left WrongProfessionSpecified{expected = Just p, specified = Just prof}
  (True, True) -> return ()

playsTam :: Square -> Square -> Side -> Operation ()
playsTam from to sid = f `validatesPlaying` (from, to, sid) where
 f Nothing = return ()
 f (Just(_, p, _))= Left WrongProfessionSpecified{expected = Just p, specified = Nothing}




-- | Drops the piece to the square. 
-- 
--    * 'NoCorrespondingPieceInHand' is raised if no pieces in the hand matches the condition given by the arguments.
--
--    * 'AlreadyOccupied' is raised if the square is already occupied by another piece.
drops :: (Color, Profession) -> Square -> Side -> Operation ()
drops (c,p) sq s = dropPiece (c,p,s) sq

-- | Similar to 'drops'', but infers the color of the piece to be dropped. 'AmbiguousColor' is raised if the color of the piece cannot be uniquely identified.
drops' :: Profession -> Square -> Side -> Operation ()
drops' p sq s = do
 fb <- get
 let kok  = dropPiece (Kok1, p, s) sq `runStateT` fb
 let huok = dropPiece (Huok2, p, s) sq `runStateT` fb
 case (kok, huok) of
  (Right _, Right _) -> lift $ Left AmbiguousColor
  (Left _, Right ((), newBoard)) -> put newBoard
  (Right ((), newBoard), Left _) -> put newBoard
  (Left NoCorrespondingPieceInHand, Left e) -> lift $ Left e
  (Left e, Left NoCorrespondingPieceInHand) -> lift $ Left e
  (Left e1, Left _) -> lift $ Left e1

-- | Skips a turn.
passes :: Side -> Operation ()
passes _ = return ()


dropPiece :: PhantomPiece -> Square -> Operation ()
dropPiece pp sq = do
 fb <- get
 let pieces = hand fb
 case filter (match pp) pieces of
  [] -> lift $ Left NoCorrespondingPieceInHand -- cannot drop
  (x:xs) -> do
   liftBoardOpFoo $ putPiece x sq -- modify the board,
   modify (\k -> k{hand = xs ++ filter (not . match pp) pieces}) -- modify the hand

-- | Wraps an operation to show that the operation must theoretically succeed but did not happen. Fails if the wrapped operation is illegal.
mun1 :: (Side -> StateT Fullboard M ()) -> Side -> Operation ()
mun1 action side = do
 fb <- get
 case action side `runStateT` fb of
  Left e -> lift $ Left e
  Right _ -> passes side -- discard the result and allow


data Dat2
 = Mun1MakMok1Hue -- ^ 無抗行処; la als
 | Kua2Kauk2Mun1Aum1 -- ^ 筆兵無傾; la ny anknish
 | Huep2Hia1 -- ^ 地心; la meunerfergal
 | Mok1Mok1 -- ^ 行行; la nienulerless
 | Dat2AIo -- ^ 王; la nermetixaler
 | Saup1 -- ^ 獣; la pysess
 | HuetKaikADat2 -- ^ 闇戦之集; la phertarsa'd elmss
 | Cuop2Mun1Mok1Hue -- ^ 声無行処; la ytartanerfergal
-- ~ | BapPok -- ^ 同色; la dejixece
 deriving(Show, Eq, Ord)

-- | Declares a Dat2. Fails with 'FalseDeclaration' if the condition required for the declaration is not met.
declare :: Side -> Dat2 -> Operation ()
declare s Mun1MakMok1Hue = declare' s [Nuak1, Kauk2, Gua2, Kaun1, Dau2, Maun1, Kua2, Tuk2, Uai1, Io]
declare s Kua2Kauk2Mun1Aum1 = declare' s [兵, 弓, 将, 筆, 巫]
declare s Huep2Hia1 = declare' s [将, 筆, 巫]
declare s Mok1Mok1 = declare' s [船, 車, 馬]
declare s Dat2AIo = declare' s [王]
declare s Saup1 = declare' s [虎, 馬]
declare s HuetKaikADat2 = declare' s [兵,兵,兵,兵,兵]
declare s Cuop2Mun1Mok1Hue = declare'' s ((>=10) . length)

type Validator2 = [(Color, Profession)] -> Bool

declare'' :: Side -> Validator2 -> Operation ()
declare'' s_ f = do
 Fullboard{hand = h} <- get
 let cplist = [ (c,p) | Just(c,p,s) <- map toPhantom h, s == s_]
 if f cplist then return () else lift $ Left FalseDeclaration

declare' :: Side -> [Profession] -> Operation ()
declare' s_ condition = declare'' s_ (matchHand condition . map snd)

matchHand :: [Profession] -> [Profession] -> Bool
matchHand condition plist = (length condition - S.size int) <= (S.occur Io plist' - S.occur Io int)
  where
   cond'  = S.fromList condition
   plist' = S.fromList plist
   int = S.intersection cond' plist'
{- 
 | cond' `S.isSubsetOf` plist' = True -- if subset, very good!
 | Io `notElem` plist = False -- if no wildcard, then nothing to worry
 | otherwise -} 

{-
length condition - S.size int :: how many pieces must be accounted for by Io?
S.occur Io plist' - S.occur Io int :: how many Io do we have at our disposal?
-}


taxot1 :: Operation ()
taxot1 = return ()

