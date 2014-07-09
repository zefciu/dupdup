module Contract where

data Suit = Clubs | Diamonds | Hearts | Spades | NoTrump deriving Eq

instance Show Suit where
    show Clubs = "♣"
    show Diamonds = "♢"
    show Hearts = "♡"
    show Spades = "♠"
    show NoTrump = "NT"

data SuitType = Minor | Major | NoTrumpType deriving (Show, Eq)

suitType :: Suit -> SuitType
suitType Clubs = Minor
suitType Diamonds = Minor
suitType Hearts = Major
suitType Spades = Major
suitType NoTrump = NoTrumpType


data Side = N | S | E | W deriving (Show, Eq)

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

baseTrickPoints :: Bool -> Contract -> Int
baseTrickPoints all c
    |st == Minor = tricks * 20
    |st == Major = tricks * 30
    |st == NoTrumpType = tricks * 30 + 10
    where
        st = suitType $ suit c
        tricks = if all then ((taken c) - 6) else (bid c)

trickPoints :: Bool -> Contract -> Int
trickPoints all c
    |doubl c == NotDoubled = base
    |doubl c == Doubled = base * 2
    |doubl c == Redoubled = base * 4
    where base = baseTrickPoints all c

bidTrickPoints = trickPoints False
allTrickPoints = trickPoints True

newtype Bonus = Contract -> Int

contractBonus :: Bonus
contractBonus c
    | bidTrickPoints c < 0 = 50
    | vulnerable c = 500
    | otherwise = 300

insultBonus :: Bonus
insultBonus c = case (doubl c) of
    NotDoubled -> 0
    DoubleLevel -> 50
    Redoubled -> 100

slamBonus :: Bonus
slamBonus c =
    bid c < 6 | 0
    bid c == 6 && not $ vulnerable c | 500
    bid c == 6 && vulnerable c | 750
    bid c == 7 && not $ vulnerable c | 1000
    bid c == 7 && vulnerable c | 1500

doneContractPoints :: Contract -> Int
doneContractPoints = (
    allTrickPoints c +
    contractBonus c +
    insultBonus c +
    slamBonus c
)
