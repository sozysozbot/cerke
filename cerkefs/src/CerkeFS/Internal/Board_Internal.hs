module CerkeFS.Internal.Board_Internal
(Square2
,Internal
,iLookup
,iInsert
,iMember
,iDelete
,Board1(..)
)where
import CerkeFS.Piece3
import qualified Data.IntMap as I

newtype Board1 = Board1{ unBoard1 :: Internal } deriving(Show, Eq, Ord)
type Square2 = Int

type Internal = I.IntMap Piece

iLookup :: Int -> Internal -> Maybe Piece
iLookup = I.lookup

iInsert :: Int -> Piece -> Internal -> Internal
iInsert = I.insert

iMember :: Int -> Internal -> Bool
iMember = I.member

iDelete :: Int -> Internal -> Internal
iDelete = I.delete
