module RandomPlay
(main
)
where
import CerkeFS
import Data.Either(isRight)
import System.Random.MWC
import PseudoStateT

dispatch :: Move -> Side -> Fullboard -> Either Error Fullboard
dispatch (Move2 from to) sid fb = vPlays2 from to sid `execStateT` fb
dispatch (Move3 from thru to) sid fb = vPlays3 from thru to sid `execStateT` fb
dispatch (Drop (c,p) sq) sid fb = drops (c,p) sq sid `execStateT` fb

canDeclare :: Side -> Fullboard -> [Dat2]
canDeclare sid fb = [ dat | dat <- [Mun1MakMok1Hue .. Cuop2Mun1Mok1Hue], isRight (declare sid dat `runStateT` fb)] 



randomPlay2_ :: App ()
randomPlay2_ = do
 coin <- lift . withSystemRandom . asGenIO $ \gen -> uniformR (0, 1) gen
 [] <- if coin == (0 :: Int) then randomlyPlayOnce_ Downward else return []  
 randomPlay_

randomPlay_ :: App ()
randomPlay_ = do
 arr1 <- randomlyPlayOnce_ Upward
 if(null arr1) then do
   arr2 <- randomlyPlayOnce_ Downward
   if(null arr2) then randomPlay_ else lift $ print arr2
 else lift $ print arr1

type App = PseudoStateT Fullboard

randomlyPlayOnce_ :: Side -> App [Dat2]
randomlyPlayOnce_ sid = pseudoStateT $ \fb -> do
 let (list, alpha, beta, gamma) = testAll' sid fb
 i <- withSystemRandom . asGenIO $ \gen -> uniformR (0, length list - 1) gen
 let move = list !! i
 case move of
  Move2 from to -> putStrLn $ unwords["vPlays2",show from, show to,show sid]
  Move3 from thru to -> putStrLn $ unwords["vPlays3",show from, show thru, show to, show sid]
  Drop (c,p) sq -> putStrLn $ unwords["drops", show (c,p), show sq, show sid]
 appendFile "count_candidate_2.txt" $ unwords (map show [alpha+beta+gamma, alpha, beta, gamma]) ++ "\n"
 let Right new_fb = dispatch move sid fb
 return (canDeclare sid new_fb,new_fb)

main :: IO ()
main = do
 ((), Fullboard{board = final, hand = pieces}) <- runPseudoStateT randomPlay2_ initialFullBoard
 putStrLn $ drawBoard final ++ "~~~\n" ++ concatMap (convertPieceToStr . toFullPiece) pieces ++ "\n"
