import Data.List (find)

type BallsInBins = [[Char]]
type Move = (Int, Int)

start :: BallsInBins
start = ["///", "abc", "de"]

end :: BallsInBins
end =  ["bc/", "da/", "e/"]

main :: IO ()
main = print $ itter [((start, 0), Nothing)]

findMatchingInt :: BallsInBins -> [((BallsInBins, Int), Maybe [Move])] -> Maybe Int
findMatchingInt target binsList =
    case find (\((bin, _), _) -> bin == target) binsList of
        Just ((_, int), _) -> Just int
        _ -> Nothing

itter :: [((BallsInBins, Int), Maybe [Move])] -> Either Int [((BallsInBins, Int), Maybe [Move])]
itter arr =
    case findMatchingInt end arr of
        Just num -> Left num
        Nothing -> itter $ map' run arr
        where
            run :: ((BallsInBins, Int), Maybe [Move]) -> [((BallsInBins, Int), Maybe [Move])]
            run ((bibs, deepn), mayMove) =
                    case mayMove of
                        Nothing -> [((bibs, deepn), Just $ legalMoves bibs)]
                        Just moves ->
                            case moves of
                                [] -> [(oldTuple, Just [])]
                                _ ->
                                    case findMatchingInt transBins arr of
                                        Just num ->
                                            if deepn >= num || end == transBins
                                                then [(oldTuple, Just $ tail moves)]
                                                else transArray
                                        Nothing -> transArray
                            where
                                transBins = transform (head moves) bibs
                                oldTuple = (bibs, deepn)
                                transArray = [(oldTuple, Just $ tail moves), ((transBins, deepn + 1), Nothing)]

map' :: (a -> [a]) -> [a] -> [a]
map' _ []     = []
map' f (x:xs) = f x ++ map' f xs

legalMoves :: BallsInBins -> [Move]
legalMoves bibs = foldr forFold [] allMoves
    where
        forFold :: Move -> [Move] -> [Move]
        forFold (fs, sn) acc =
            if (content (bibs !! fs) > 0) && (content sndElem < length sndElem)
                then (fs, sn):acc
                else acc
                    where sndElem = bibs !! sn

content :: String -> Int
content str = length $ filter (/= '/') str

allMoves :: [Move]
allMoves = [(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)]

transform :: Move -> BallsInBins -> BallsInBins
transform (from, to) bib = replaceNth to to'' $ replaceNth from from'' bib
    where
        from' = bib !! from
        to' = bib !! to
        (from'', char) = pop from'
        to'' = push char to'

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal list =
  case splitAt n list of
    (before, _:after) -> before ++ newVal : after
    _ -> list

push :: Char -> [Char] -> [Char]
push '/' _ = error "Cant push empty"
push _ [] = error "Cant exceed length"
push char (c:cs)
    | c == '/' = char:cs
    | otherwise = c : push char cs

pop :: [Char] -> ([Char], Char)
pop [] = ([], '/')
pop cs
    | last cs == '/' = (arr ++ "/", ch)
    | otherwise = (reverse $ '/' : as, a)
        where
            (a:as) = reverse cs
            (arr, ch) = pop (init cs)
