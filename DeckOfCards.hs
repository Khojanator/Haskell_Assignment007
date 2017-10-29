module DeckOfCards where

data Suit = Diamonds | Clubs | Hearts | Spades
            deriving (Eq, Show, Ord, Enum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq, Show, Ord, Enum)

data PlayingCard = Card Suit Rank | Joker -- extended to support 5.9
            deriving (Eq, Show, Ord)

-- produces deck of cards using list comprehension, for fun
cardDeck :: [PlayingCard]
cardDeck = [(Card st rnk) | st <- [Diamonds ..], rnk <- [Two ..]]

--returns the Suit of a card
getSuit :: PlayingCard -> Suit
getSuit (Card st rnk) = st

getRank :: PlayingCard -> Rank
getRank (Card st rnk) = rnk

{-
getRankInt :: PlayingCard -> Int
getRankInt
-}

whoWinsRound :: PlayingCard -> PlayingCard -> PlayingCard
whoWinsRound (Card st1 rnk1) (Card st2 rnk2)
    | rnk1 > rnk2   = (Card st1 rnk1)
    | rnk2 > rnk1   = (Card st2 rnk2)
   -- | rnk1 == rnk2  = runSomething!

-- Function that determines which card would win.
whoWins :: PlayingCard -> PlayingCard -> PlayingCard
whoWins Joker _ = Joker
whoWins _ Joker =  Joker
whoWins (Card st1 rnk1) (Card st2 rnk2)  
    | st1 > st2    = (Card st1 rnk1)
    | st2 > st1    = (Card st2 rnk2)
    | rnk1 > rnk2  = (Card st1 rnk1)
    | rnk2 > rnk1  = (Card st2 rnk2)
    | otherwise    = (Card st1 rnk1)

-- fullSuit returns the cards from the deck of cards that belong to that suit.
fullSuit :: Suit -> [PlayingCard]
fullSuit st0 = [a | a <- cardDeck, getSuit a == st0]


