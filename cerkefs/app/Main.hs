module Main 
(module Main
)
where
import CerkeFS
import Data.Either(isRight)
import System.Random.MWC

dispatch :: Move -> Side -> Fullboard -> Either Error Fullboard
dispatch (Move2 from to) sid fb = vPlays2 from to sid `execStateT` fb
dispatch (Move3 from thru to) sid fb = vPlays3 from thru to sid `execStateT` fb
dispatch (Drop (c,p) sq) sid fb = drops (c,p) sq sid `execStateT` fb

canDeclare :: Side -> Fullboard -> [Dat2]
canDeclare sid fb = [ dat | dat <- [Mun1MakMok1Hue .. Cuop2Mun1Mok1Hue], isRight (declare sid dat `runStateT` fb)] 


main :: IO ()
main = do
 ((), Fullboard{board = final, hand = pieces}) <- runStateT randomPlay initialFullBoard
 putStrLn $ drawBoard final ++ "~~~\n" ++ concatMap convertPieceToStr pieces ++ "\n"

randomPlay :: StateT Fullboard IO ()
randomPlay = do
 coin <- lift . withSystemRandom . asGenIO $ \gen -> uniformR (0, 1) gen
 [] <- if coin == (0 :: Int) then randomlyPlayOnce Downward else return []
 randomPlay'

randomPlay' :: StateT Fullboard IO ()
randomPlay' = do
 arr1 <- randomlyPlayOnce Upward
 if(null arr1) then do
   arr2 <- randomlyPlayOnce Downward
   if(null arr2) then randomPlay' else lift $ print arr2
 else lift $ print arr1


randomlyPlayOnce :: Side -> StateT Fullboard IO [Dat2]
randomlyPlayOnce sid = StateT $ \fb -> do
 let list = testAll sid fb
 i <- withSystemRandom . asGenIO $ \gen -> uniformR (0, length list - 1) gen
 let move = list !! i
 print $ (move, length list)
 let Right new_fb = dispatch move sid fb
 return (canDeclare sid new_fb,new_fb)




 --print $ toDebugOutput $ do{vPlays3' sqKE  Tuk2 sqLE sqNE Downward; vPlays2' sqTAI Kauk2 sqTY Upward}
 {-
 foo "test" $ do
  -- vPlays2' sqTAI Kauk2 sqTY  >+> vPlays2' sqXI Kauk2 sqXU
  -- vPlays2' sqXAI Kauk2 sqXY  >+> vPlays2' sqZI Nuak1 sqZU
  -- vPlays2' sqZAI Nuak1 sqZY  >+> playsTam sqZO     sqXI
  -- vPlays2' sqXAU Dau2  sqZAI >+> vPlays3' sqXA Uai1 sqXE sqZI
  -- vPlays2' sqMAU Gua2  sqZAU >+> vPlays3' sqLE Gua2 sqTE sqZE
  -- vPlays3' sqPAU Tuk2  sqPAI sqPY  >+> vPlays3' sqXE Dau2 sqZI sqTU
  -- vPlays2' sqNAI Kauk2 sqNY  >+> vPlays3' sqTA Uai1  sqZE sqXE
  -- vPlays2' sqCAI Kauk2 sqCY  >+> vPlays2' sqXU Kauk2 sqXY
  -- vPlays2' sqZAI Dau2  sqXY  >+> vPlays2' sqTU Dau2  sqXY
  -- mun1(vPlays2' sqTAU Dau2  sqZAI) >+> vPlays3' sqME Gua2 sqMI sqMU
  -- vPlays2' sqMAI Kauk2 sqMY  >+> vPlays2' sqXY Dau2  sqPIA
  -- vPlays2' sqTAU Dau2  sqNAI >+> vPlays2' sqZU Nuak1 sqZY
  -- vPlays2' sqZAU Gua2  sqZY  >+> vPlays3' sqMU Gua2  sqMY sqMIA
  -- vPlays3' sqCIA Kaun1 sqXAU sqZAI >+> vPlays2' sqMIA Gua2 sqZIA
  -- declare Downward Dat2AIo 
  taxot1 -}

foo :: String -> Operation a2 -> IO ()
foo str fed = do
 putStrLn str
 putStrLn (toDebugOutput fed)
 print (toDebugOutput fed)



{-
loadFile :: FilePath -> IO Board1
loadFile file = do
 str <- readFile file
 let Just b = loadBoard str
 return b

loadFile' :: String -> IO ()
loadFile' file = do
 putStrLn "--------------------"
 putStrLn $ "Loading " ++ file
 str <- readFile file
 putStrLn str
 putStrLn ""
 print $ loadBoard str
 putStrLn ""
 putStrLn "Reverse:"
 let Just b = loadBoard str
 putStrLn $ drawBoard b
-}






