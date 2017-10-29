-- Ahsan Ali Khoja & Rei Rembeci

import System.Random
import DeckOfCards

-- Hello, World
helloWorld :: IO()
helloWorld = putStrLn "Hello, World"

-- Hello, Name
helloName :: IO()
helloName = do
    putStrLn "Hey, what is your name?"
    thisName <- getLine
    putStrLn ("Hello, " ++ thisName)

-- Magic Eight-Ball Toy
eightBall :: IO()
eightBall = do
    putStrLn "Welcome to the world of Magic Eight-Ball. Ask what your heart wishes..."
    doesntMatter <- getLine
    randomGen <- newStdGen
    let randNum = fst $ randomR (0, ((length responses) - 1)) randomGen
    putStrLn (responses !! randNum)
    where
        responses = ["Yeppidy Duppidy Dup!", "You got this, homie!", "Fishes food", "Life is beautiful, and so are you", "Keep on trying", "Nah, maybe try some other thing", "42"]

ruffleShuffle :: [PlayingCard] -> [PlayingCard] -> [PlayingCard]
ruffleShuffle [] [] = []
ruffleShuffle (x:xs) (y:ys) = [x,y] ++ (ruffleShuffle xs ys)

splitList :: [PlayingCard] -> ([PlayingCard],[PlayingCard])
splitList listOfCards = (listA, listB)
    where
        listA = take (quot (length listOfCards) 2) listOfCards
        listB = [x | x <- listOfCards, not (elem x listA)]

shuffle :: [PlayingCard] -> IO [PlayingCard]
shuffle listX = do
    let (a,b) = splitList listX
    randomNum <- newStdGen
    let shuffleDecider = fst $ randomR (0,1) randomNum
    if (shuffleDecider == (1::Int)) then 
        return (ruffleShuffle a b)
    else
        return (ruffleShuffle b a)

masterShuffle :: [PlayingCard] -> IO [PlayingCard]
masterShuffle listX = do
    s1 <- shuffle listX
    s2 <- shuffle s1
    s3 <- shuffle s2
    s4 <- shuffle s3
    s5 <- shuffle s4
    s6 <- shuffle s5
    s7 <- shuffle s6
    s8 <- shuffle s7
    s9 <- shuffle s8
    return s9

divideCardsWar :: [PlayingCard] -> ([PlayingCard],[PlayingCard])
divideCardsWar listX = (listA, listB)
    where
        listA = [listX !! (2*x) | x <- [0..((quot (length listX) 2)-1)]]
        listB = [y | y <- listX, not(elem y listA)]

stepWar :: [PlayingCard] -> [PlayingCard] -> ([PlayingCard], [PlayingCard])
stepWar (x:xs) (y:ys)
    | (getRank x) > (getRank y)    = ((xs ++ [x,y]), ys)
    | (getRank x) < (getRank y)    = (xs, (ys ++ [y,x]))
    | (getRank x) == (getRank y)     = warOnWar (x:xs) (y:ys) 1

preWar :: [PlayingCard] -> [PlayingCard] -> Int -> ([PlayingCard], [PlayingCard])
preWar (x:xs) (y:ys) a
    | ((length xs) < (4*a)) && ((length xs) < (length ys))  = ([], ((y:ys) ++ (x:xs)))
    | ((length ys) < (4*a)) && ((length xs) > (length ys))  = (((x:xs) ++ (y:ys)), [])
    | ((length xs) == 0) || ((length ys) == 0) || ((getRank(last xs)) == (getRank(last ys)))  = ((x:xs),(y:ys))
    | (getRank(last xs)) > (getRank(last ys))     = (((x:xs) ++ (y:ys)), [])
    | (getRank(last xs)) < (getRank(last ys))     = ([], ((y:ys) ++ (x:xs)))

warOnWar :: [PlayingCard] -> [PlayingCard] -> Int -> ([PlayingCard], [PlayingCard])
warOnWar (x:xs) (y:ys) a
    | ((length xs) < (4*a)) || ((length ys) < (4*a))     = preWar (x:xs) (y:ys) a
    | (getRank (xs !! (4*a - 1))) > (getRank (ys !! (4*a - 1)))     = (remX ++ gain, remY)
    | (getRank (xs !! (4*a - 1))) < (getRank (ys !! (4*a - 1)))     = (remX, remY ++ gain)
    | (getRank (xs !! (4*a - 1))) == (getRank (ys !! (4*a - 1)))     = warOnWar (x:xs) (y:ys) (a+1)
    where
        remX = [t | t <- xs, not(elem t (take (4*a) xs))]
        remY = [t | t <- ys, not(elem t (take (4*a) ys))]
        gain = [x,y] ++ (take (4*a) xs) ++ (take (4*a) ys)

autoWar :: IO ()
autoWar = do
    putStrLn "Welcome! Let the War(s) begin"
    newDeck <- masterShuffle cardDeck
    let (handA, handB) = divideCardsWar newDeck
    putStrLn "|       Player A       |       Player B       |"
    putStrLn "| # of Cards | Card In | Card In | # of Cards |"
    autoWar' handA handB
    putStrLn "Game ended. Fishes Food."

autoWar' :: [PlayingCard] -> [PlayingCard] -> IO ()
autoWar' (x:xs) (y:ys) = do
    let (handA, handB) = stepWar (x:xs) (y:ys) 
    putStrLn ("| " ++ show (length xs) ++ " | " ++ show x ++ " | " ++ show y ++ " | " ++ show (length ys) ++ " |")
    if ((handA /= []) && (handB /= []) && (handA /= (x:xs)) && (handB /= (y:ys)))
        then autoWar' handA handB
    else endWar handA handB

endWar :: [PlayingCard] -> [PlayingCard] -> IO ()
endWar [] _ = putStrLn "Player B wins!"
endWar _ [] = putStrLn "Player A wins!"
endWar _ _ = putStrLn "~~~ Let the Peace prevail! ~~~"

interactiveWar :: IO ()
interactiveWar = do
    putStrLn "Welcome! Let the War(s) begin"
    newDeck <- masterShuffle cardDeck
    let (handA, handB) = divideCardsWar newDeck
    putStr "Please enter your name: "
    userName <- getLine
    putStrLn ("|       " ++ userName ++ "       |       Computer       |")
    putStrLn "| # of Cards | Card In | Card In | # of Cards |"
    interactiveWar' handA handB
    putStrLn "Game ended. Fishes Food."

interactiveWar' :: [PlayingCard] -> [PlayingCard] -> IO ()
interactiveWar' (x:xs) (y:ys) = do
    let (handA, handB) = stepWar (x:xs) (y:ys) 
    putStrLn ("| " ++ show (length xs) ++ " | " ++ show x ++ " | " ++ show y ++ " | " ++ show (length ys) ++ " |")
    putStr "Press Enter to play your next card!"
    userIn <- getLine
    if ((handA /= []) && (handB /= []) && (handA /= (x:xs)) && (handB /= (y:ys)))
        then interactiveWar' handA handB
    else endInteractiveWar handA handB

endInteractiveWar :: [PlayingCard] -> [PlayingCard] -> IO ()
endInteractiveWar [] _ = putStrLn "You lose :("
endInteractiveWar _ [] = putStrLn "You are the winner! :D"
endInteractiveWar _ _ = putStrLn "~~~ Let the Peace prevail! ~~~"