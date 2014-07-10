module Contract where

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

data DoubleLevel = NotDoubled | Doubled | Redoubled deriving (Show, Eq)

data Contract = Contract {
    board :: Int,
    player :: Side,
    bid :: Int,
    suit :: Suit,
    doubl :: DoubleLevel,
    taken :: Int
} deriving (Show, Eq)

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
