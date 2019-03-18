import System.Random
import System.IO

type Column     = Int
type Row        = Int
type CellValue  = Int
type Spawning   = (Column, Row, CellValue)
type CellMatrix = [[Int]]

data Command = 
    Upward 
  | Downward 
  | Rightward 
  | Leftward 
  deriving (Eq, Show)

data Game = Game {
    side      :: Int,
    matrix    :: CellMatrix,
    spawnings :: [Spawning],
    score     :: Int,
    gameOver  :: Bool
} deriving (Eq, Show)

initGame :: Int -> Game
initGame side = Game side cm spwns 0 False where 
    cm    = spawn . spawn . spawn $ replicate side $ replicate side 0
    spwns = zipWith3 (\col row cellVal -> (col, row, cellVal)) xs ys vs
    xs    = (mod <$> (randoms (mkStdGen 0))) <*> (pure side)
    ys    = (mod <$> (randoms (mkStdGen 1))) <*> (pure side)
    vs    = (\x -> x * 2 + 2) <$> ((mod <$> (randoms (mkStdGen 2))) <*> (pure 2))
    spawn = spawnCell spwns

insert :: [a] -> Int -> a -> [a]
insert [] _ _      = [] 
insert (x:xs) 0 x' = x' : xs
insert (x:xs) n x' = x : insert xs (n-1) x'

spawnCell :: [Spawning] -> CellMatrix -> CellMatrix
spawnCell [] cm = cm
spawnCell ((x, y, v):spawnTail) cm = cm' where
    cm' = case hasEmptyCell cm of 
        False -> cm 
        True  -> case empty cm x y of
            True  -> insert' cm x y v
            False -> spawnCell spawnTail cm
    empty cm x y      = ((cm !! y) !! x) == 0
    insert' cm x y v  = insert cm y $ insert (cm !! y) x v
    hasEmptyCell cm   = foldr (\xs acc -> (elem 0 xs) || acc) False cm

accumulateRow :: Int -> [(Int, Bool)] -> [(Int, Bool)]
accumulateRow x [] = [(x, False)]
accumulateRow x xpms@((x', prevMerged):tail)
    | x == 0                                   = xpms  
    | (x == x' || x' == 0) && (not prevMerged) = (x' + x, x == x') : tail
    | otherwise                                = (x, False) : xpms

merge :: Int -> CellMatrix -> CellMatrix
merge side cm = foldr f [] cm where
    f row acc = ((replicate (side - (length mergedRow)) 0) ++ 
                (fst <$> mergedRow)) : acc where
                    mergedRow = foldr accumulateRow [] row

transpose :: CellMatrix -> CellMatrix
transpose []     = []
transpose ([]:_) = []
transpose cm     = transposeColumn cm : (transpose $ removeColumn <$> cm) where
    removeColumn []       = []
    removeColumn (_:xs)   = xs  
    transposeColumn cm    = foldr accumColumn [] cm 
    accumColumn [] _      = []
    accumColumn (x:_) acc = x:acc

mirrorAtHorizontalAxis :: CellMatrix -> CellMatrix
mirrorAtHorizontalAxis cm = reverse cm 

mirrorAtVericalAxis :: CellMatrix -> CellMatrix
mirrorAtVericalAxis cm = foldr (\x acc -> (reverse x) : acc) [] cm

orientLeft :: CellMatrix -> CellMatrix
orientLeft cm = mirrorAtVericalAxis cm

orientDown :: CellMatrix -> CellMatrix
orientDown cm = mirrorAtHorizontalAxis . transpose $ cm

orientUp :: CellMatrix -> CellMatrix
orientUp cm = mirrorAtVericalAxis . transpose $ cm

orientRight :: CellMatrix -> CellMatrix
orientRight cm = id cm

execute :: Command -> Game -> Game
execute _ g@(Game _ _ _ _ True) = g
execute cmd (Game side cm spwns@(_:spawnTail) sc _) =  
    Game side nextCm spawnTail (sumScore cm') (gameOver nextCm) where
        nextCm = if cm /= cm' then spawnCell spwns cm' else cm'
        cm' = case cmd of
            Leftward  -> orientLeft . mergeMatrix . orientLeft $ cm
            Upward    -> orientDown . mergeMatrix . orientUp $ cm
            Downward  -> orientUp . mergeMatrix . orientDown $ cm
            Rightward -> orientRight . orientRight . mergeMatrix $ cm

        mergeMatrix         = merge side
        sumScore xs         = sum $ foldr (\x acc -> (sum x) : acc) [] xs
        gameOver cm         = not $ anyColumnsMergeable cm || anyRowsMergeable cm
        anyRowsMergeable    = foldr (\xs acc -> (anyCellsMergeable xs) || acc) False
        anyColumnsMergeable = anyRowsMergeable . transpose
        anyCellsMergeable   = 
            snd . foldr (\x (prev, acc) -> (x, acc || ((prev == x) || x == 0))) (-1, False)

gameLoop :: Game -> IO ()
gameLoop g@(Game side _ _ score True) = do
    putStrLn "Game Over!"
    putStrLn $ "Your final score is: " ++ show score
    putStrLn "Press 'r' to restart the game or 'q' to quit."
    putStr ">"
    cmd <- getLine
    case cmd of
        "r" -> gameLoop $ initGame side
        "q" -> return ()
        _   -> do
            putStrLn $ "Couldn't catch: " ++ cmd
            gameLoop g

gameLoop g@(Game side cm _ score _) = do
    putStrLn $ "Your score is: " ++ show score
    mapM_ print cm
    putStr ">"
    cmd <- getLine
    case cmd of
        "w" -> gameLoop $ execute Upward g
        "a" -> gameLoop $ execute Leftward g
        "s" -> gameLoop $ execute Downward g
        "d" -> gameLoop $ execute Rightward g
        "q" -> return ()
        "r" -> gameLoop $ initGame side
        _   -> do
            putStrLn $ "Couldn't catch: " ++ cmd
            gameLoop g

main :: IO ()
main = do
    putStrLn "Let's play 2048!"
    putStrLn "Press 'w' 'a' 's' 'd' to merge up, left, down or right respectively."
    putStrLn "Press 'r' to restart the game or 'q' to quit."
    gameLoop $ initGame 4
