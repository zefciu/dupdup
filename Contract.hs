module Contract where

import Data.String.Utils (join)
import Data.List.Split (splitOn)

data Suit = Clubs | Diamonds | Hearts | Spades | NoTrump deriving Eq

instance Show Suit where
    show Clubs = "♣"
    show Diamonds = "♢"
    show Hearts = "♡"
    show Spades = "♠"
    show NoTrump = "NT"

instance Read Suit where
    readsPrec _ ('♣' : xs) = [(Clubs, xs)]
    readsPrec _ ('C' : xs) = [(Clubs, xs)]
    readsPrec _ ('♢' : xs) = [(Diamonds, xs)]
    readsPrec _ ('D' : xs) = [(Diamonds, xs)]
    readsPrec _ ('♡' : xs) = [(Hearts, xs)]
    readsPrec _ ('H' : xs) = [(Hearts, xs)]
    readsPrec _ ('♠' : xs) = [(Spades, xs)]
    readsPrec _ ('S' : xs) = [(Spades, xs)]
    readsPrec _ ('N' : 'T' : xs) = [(NoTrump, xs)]

data SuitType = Minor | Major | NoTrumpType deriving (Show, Eq)

suitType :: Suit -> SuitType
suitType Clubs = Minor
suitType Diamonds = Minor
suitType Hearts = Major
suitType Spades = Major
suitType NoTrump = NoTrumpType


data Side = N | S | E | W deriving (Show, Read, Eq)

data Pair = NS | EW deriving (Show, Eq)
pair :: Side -> Pair
pair N = NS
pair S = NS
pair E = EW
pair W = EW

opposite :: Pair -> Pair
opposite NS = EW
opposite EW = NS

vuln :: Int -> Pair -> Bool
vuln board pair
    | shiftedPos == 3 = True
    | shiftedPos == 1 && pair == NS = True
    | shiftedPos == 2 && pair == EW = True
    | otherwise = False
    where
          board0 = board - 1
          (cycle, pos) = board0 `divMod` 4
          shiftedPos = (pos + cycle) `mod` 4

data DoubleLevel = NotDoubled | Doubled | Redoubled deriving (Eq)

instance Show DoubleLevel where
    show NotDoubled = ""
    show Doubled = "x"
    show Redoubled = "xx"

instance Read DoubleLevel where
    readsPrec _ ('x' : 'x' : xs) = [(Redoubled, xs)]
    readsPrec _ ('x' : xs) = [(Doubled, xs)]
    readsPrec _ (xs) = [(NotDoubled, xs)]

parseBidStr :: String -> [(Int, Suit, DoubleLevel)]
parseBidStr s = do
    (b, s1) <- reads s
    (su, s2) <- reads s1
    (d, _) <- reads s2
    return (b, su, d)

data Contract = Contract {
    board :: Int,
    player :: Side,
    bid :: Int,
    suit :: Suit,
    doubl :: DoubleLevel,
    taken :: Int
} deriving (Eq)

instance Show Contract where
    show c = join "," [
        show $ board c,
        show $ player c,
        shows (bid c) (shows (suit c) (show (doubl c))),
        show $ taken c
        ]

instance Read Contract where
    readsPrec _ s =
        [(Contract (read board) (read player) bid suit doubl (read taken),
          "")]
        where [board, player, bidStr, taken] = splitOn "," s
              (bid, suit, doubl) = head $ parseBidStr bidStr

vulnerable :: Contract -> Bool
vulnerable c = vuln (board c) (pair $ player c)

overUnder :: Contract -> Int
overUnder c = (taken c) - (6 + bid c)



baseTrickPoints :: Suit -> Int
baseTrickPoints s
    |st == Minor = 20
    |st == Major = 30
    |st == NoTrumpType =  30
    where
        st = suitType s

type PointsForContract = (Contract -> Int)

bidTrickPoints :: PointsForContract
bidTrickPoints c = baseTrickPoints (suit c) * (bid c) + firstTrick
    where firstTrick = if (suit c == NoTrump) then 10 else 0

overTrickBonus :: PointsForContract
overTrickBonus c
    | doubl c == NotDoubled = baseTrickPoints (suit c) * (overUnder c)
    | doubl c == Doubled && not (vulnerable c) = (overUnder c) * 100
    | doubl c == Doubled && vulnerable c = (overUnder c) * 200
    | doubl c == Redoubled && not (vulnerable c) = (overUnder c) * 200
    | doubl c == Redoubled && vulnerable c = (overUnder c) * 400

contractBonus :: PointsForContract
contractBonus c
    | bidTrickPoints c < 0 = 50
    | vulnerable c = 500
    | otherwise = 300

insultBonus :: PointsForContract
insultBonus c = case (doubl c) of
    NotDoubled -> 0
    Doubled -> 50
    Redoubled -> 100

slamBonus :: PointsForContract
slamBonus c
    | bid c < 6 = 0
    | bid c == 6 && not (vulnerable c) = 500
    | bid c == 6 && vulnerable c = 750
    | bid c == 7 && not (vulnerable c) = 1000
    | bid c == 7 && vulnerable c = 1500

doneContractPoints :: PointsForContract
doneContractPoints c = bidTrickPoints c +
                       overTrickBonus c + 
                       contractBonus c +
                       insultBonus c +
                       slamBonus c

undoubledUnderTrick :: Int -> Bool -> Int
undoubledUnderTrick under vuln = under * perTrick
    where perTrick = if (vuln) then 100 else 50

doubledUnderTrick :: Int -> Bool -> Int
doubledUnderTrick under vuln
    | under == 1 = if (vuln) then 200 else 100
    | under < 4 = if (vuln)
        then 200 + (under - 1) * 300
        else 100 + (under - 1) * 200
    | under >= 4 = if (vuln)
        then 800 + (under - 3) * 800
        else 500 + (under - 3) * 300

downContractPoints :: PointsForContract
downContractPoints c
    | d == NotDoubled = undoubledUnderTrick (-(overUnder c)) (vulnerable c)
    | d == Doubled = doubledUnderTrick (-(overUnder c)) (vulnerable c)
    | d == Redoubled = 2 * (doubledUnderTrick (-(overUnder c)) (vulnerable c))
    where d = doubl c

score c 
    | overUnder c < 0 = ((opposite $ pair $ player c), downContractPoints c)
    | otherwise = ((pair $ player c), doneContractPoints c)
