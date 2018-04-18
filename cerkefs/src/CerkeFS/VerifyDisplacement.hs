module CerkeFS.VerifyDisplacement
(verifyDisplacement
) where
import CerkeFS.Internal.Board
import CerkeFS.Piece3
import CerkeFS.GameState
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Data.Maybe

isTam2Hue' :: Square -> Operation Bool
isTam2Hue' sq = do
 Fullboard{board = b}<- get
 return $ isTam2Hue b sq

moveInX, moveInPlus, moveTo8 :: [(Int,Int)]
moveInX = [(1,1),(1,-1),(-1,1),(-1,-1)]
moveInPlus = [(0,1),(1,0),(0,-1),(-1,0)]
moveTo8 = moveInX ++ moveInPlus

-- | Verify whether the given displacement is allowed by the profession.
verifyDisplacement :: Side -> Profession -> Square -> Square -> Operation ()
verifyDisplacement sid prof from to = do
 let diff = rotate sid $ to `minus` from
 isth <- isTam2Hue' from
 Fullboard{board = b} <- get
 let args = (sid, diff, from, b)
 guard' prof from $ case (isth, prof) of
  (False, Kauk2) -> simpleJudge diff [(0,1)]
  (False, Dau2 ) -> simpleJudge diff moveInX
  (False, Maun1) -> simpleJudge diff [(2,2),(2,-2),(-2,2),(-2,-2)]
  (True , Maun1) -> simpleJudge diff [(2,2),(2,0),(2,-2),(0,2),(0,-2),(-2,2),(-2,0),(-2,-2)]
  (False, Tuk2 ) -> simpleJudge diff moveInPlus
  (False, Io   ) -> simpleJudge diff moveTo8
  (True , Io   ) -> simpleJudge diff moveTo8
  (True , Uai1 ) -> simpleJudge diff moveTo8
  (False, Uai1 ) -> simpleJudge diff [(0,1),(1,0),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
  (True , Kaun1) -> simpleJudge diff moveInPlus || bishop args 
  (False, Kaun1) -> bishop args 
  (True , Dau2 ) -> bishop args
  (False, Nuak1) -> fly args (0,1)
  (True , Nuak1) -> 
   simpleJudge diff [(-1,0),(1,0)]
    || any (twoStep args) [(-1,0),(1,0)]
    || any (fly args) [(0,1),(0,-1)]
  (True , Kauk2) -> simpleJudge diff moveInPlus || twoStep args (0,1)
  (False, Gua2 ) -> rook args
  (True , Gua2 ) -> simpleJudge diff moveInX || rook args
  (False, Kua2)  -> simpleJudge diff [(1,0),(-1,0)] || any (fly args) [(0,1),(0,-1)]
  (True , Kua2)  -> rook args
  (True , Tuk2)  -> any (flyJumping0or1 args) moveTo8

type Args = (Side, Vec, Square, Board1)

rook :: Args -> Bool
rook args = any (fly args) moveInPlus

bishop :: Args -> Bool
bishop args = any (fly args) moveInX

flyFoo :: (Board1 -> [Square] -> Bool) -> Args -> (Int, Int) -> Bool
flyFoo f (sid, diff, from, b) (x,y) = (x * y1 - y * x1 == 0) && (x*x1 + y*y1 > 0) 
 && f b list
 where 
  Vec x1 y1 = diff
  vecs = takeWhile (/=(x1,y1)) [(k*x, k*y) | k <- [1..]] -- spaces in between
  list = mapMaybe ((`add` from) . rotate sid . uncurry Vec) vecs

fly :: Args -> (Int, Int) -> Bool
fly = flyFoo f where f b list = not(any (`isOccupied` b) list)

flyJumping0or1 :: Args -> (Int, Int) -> Bool
flyJumping0or1 = flyFoo f where f b list = length (filter (`isOccupied` b) list) <= 1


twoStep :: Args -> (Int, Int) -> Bool
twoStep (sid, diff, from, b) (x,y) = (x * 2 == x1) && (y * 2 == y1) 
 && not(passant `isOccupied` b)
 where 
  Vec x1 y1 = diff
  passant = fromJust $ rotate sid (Vec x y) `add` from -- cannot fail, since 'to' is guaranteed to be valid

simpleJudge :: Vec -> [(Int, Int)] -> Bool
simpleJudge diff arr = diff `elem` map (uncurry Vec) arr

guard' :: Profession -> Square -> Bool -> Operation ()
guard' prof from False = lift $ Left (ProfessionPrivilegeExceeded prof from)
guard' _ _ _ = return ()

rotate :: Side -> Vec -> Vec
rotate Downward vec = vec
rotate Upward Vec{dx=x, dy=y} = Vec{dx= -x, dy= -y}
