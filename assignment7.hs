-- Ahsan Ali Khoja & Rei Rembeci

import System.Random

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
    let randNum = fst $ randomR (0, ((length responses) - 1)) (mkStdGen (length doesntMatter))
    putStrLn (responses !! randNum)
    where
        responses = ["Yeppidy Duppidy Dup!", "You got this, homie!", "Fishes food", "Life is beautiful, and so are you", "Keep on trying", "Nah, maybe try some other thing", "42"]
