import System.IO
import Data.List

{- Pomocnicze typy danych -}
type Board = [[Int]]
type Size = (Int, Int)
type Sign = ((Int,Int),Int)
type Signs = [Sign]
type Perms = [[Int]] -- Permutacje dla jednego oznaczenia

data Creek = Creek Size Signs deriving (Read, Show)

{- Wczytanie danych z pliku (akcja) -}
readCreek :: String -> IO(Creek)
readCreek path = do f <- readFile path
                    return $ read f

{- Główna część aplikacji -}
main = do
       putStrLn "Witaj w programie do rozwiązywania łamigłówki typu Creek."
       putStrLn "Podaj nazwę pliku, z którego chcec wczytać łamigłówkę:"  
       fpath <- getLine
       putStrLn ("Odczyt " ++ fpath)
       creek <- readCreek fpath
       let size = returnCreekSize creek
       let signs = returnCreekSigns creek
       putStrLn "Odczytano poprawnie dane z pliku."
       putStrLn "Proszę czekać trwa rozwiązywanie..."
       let permsForAllSigns = permsForSign signs
       let allCombinationsOfPerms = sequence permsForAllSigns
       let generatedMaps = mergeAllMaps (newMap size (-1)) signs allCombinationsOfPerms
       let res = selectResults generatedMaps signs
       printAllMaps res

returnCreekSize :: Creek -> Size
returnCreekSize (Creek s _) = s

returnCreekSigns :: Creek -> Signs
returnCreekSigns (Creek _ s) = s

{- Generuje dla każdego wymagania sekwencje permutacji  -}
permsForSign :: Signs -> [Perms]
permsForSign [] = []
permsForSign (((_,_),n):xs) = (permOfList n) : permsForSign xs

{- Permutacje listy dla podanego wymagania (0,..,4) -}
permOfList :: Int -> [[Int]]
permOfList i | i >= 0 && i < 5 = uniquePerms ( ( replicate i 1 ) ++ ( replicate (4-i) 0 ) )
             | otherwise = error "Błędna wartość liczbowa"

{- Główny algorytm - Zwrócenie poprawnego rozwiązania -}
selectResults :: [Board] -> Signs -> [Board]
selectResults [] _ = []
selectResults (f:l) signs | isSolutionCorrect f signs && (allWhite == visitedWhite) = [newMxWithOneStream]
                          | otherwise = selectResults l signs
                          where allWhite = numOfAllWhiteFields f
                                newMxWithOneStreamAndUnmarkedFields = mxOneStreamUnmarked f
                                visitedWhite = numOfAllWhiteFields $ newMxWithOneStreamAndUnmarkedFields
                                newMxWithOneStream = minutsOneToZeroBoard newMxWithOneStreamAndUnmarkedFields

{- Sprawdzenie czy rozwiązanie spełnia wymagania zadania -}
--czy liczba zamalowanych pól wokół oznaczeń się zgadza?
isSolutionCorrect :: Board -> Signs -> Bool
isSolutionCorrect _ [] = True
isSolutionCorrect mx ((pos,n):signs) | (numOfFulfillFieldsAroud mx genCords) == n = isSolutionCorrect mx signs
                                     | otherwise = False
                                     where genCords = generateCordsAround mx pos

{- Zwraca macierz wypełnioną strumieniem wraz z nieoznaczonymi polami -}
mxOneStreamUnmarked :: Board -> Board
mxOneStreamUnmarked mx = visitWhiteUnmarked (selectFirstWhite mx) mx (newMap (nmy,nmx) 1)
                         where nmy = length mx
                               nmx = length $ head mx

{- Zlicza wypełnione pola wokół danego oznaczenia -}
numOfFulfillFieldsAroud :: Board -> [(Int, Int)] -> Int
numOfFulfillFieldsAroud mx [] = 0
numOfFulfillFieldsAroud mx ((y,x):cords) = elem + numOfFulfillFieldsAroud mx cords
                                  where elem = mx !! y !! x

{- Generuje zbiór koordynatów wokół danego wymagania jeśli mieszczą się w mapie -}
generateCordsAround :: Board -> (Int, Int) -> [(Int, Int)]
generateCordsAround mx (y,x) = [x | x <- cords, isCordsInsideMap mx x]
                              where cords = [(y-1, x-1), (y-1, x), (y, x-1), (y, x)]

--Policzenie niezamalowanych pól
numOfAllWhiteFields :: Board -> Int
numOfAllWhiteFields [] = 0
numOfAllWhiteFields (f:mx) = length ( filter (==0) f ) + numOfAllWhiteFields mx

{- Wybranie pierwszego białego pola z listy jeśli istnieje i zwrócenie jego pozycji -}
selectFirstWhite :: Board -> (Int, Int)
selectFirstWhite mx = (row, col)
                     where idxList = checkIndex $ elemIndex 0 (convMxToList mx)
                           ny = length $ head mx
                           col = idxList `mod` ny
                           row = idxList `div` ny

{- Sprawdzenie czy znaleziono jakiekolwiek biale pole -}
checkIndex :: Maybe Int -> Int
checkIndex ii = case ii of
                       Just n  -> n
                       Nothing -> error "Brak wskazanego pola - brak strumienia"

{- Odwiedzenie wszystkich białych pól tworzących wyspę i zwrócenie planszy zawierającej 0,1,-1 
zaczynając od podanej pozcji na planszy -}
visitWhiteUnmarked :: (Int, Int) -> Board -> Board -> Board
visitWhiteUnmarked (row, col) mx visited | isCordsInsideMap mx (row, col) && ( mxF /= 1) && ( vF == 1 )
                                 = visitWhiteUnmarked (row, col+1) mx $
                                   visitWhiteUnmarked (row, col-1) mx $
                                   visitWhiteUnmarked (row+1, col) mx $
                                   visitWhiteUnmarked (row-1, col) mx nvisited 
                                 | otherwise = visited
                                 where mxF = mx !! row !! col
                                       vF = visited !! row !! col
                                       nvisited = updateBoard visited mxF (row, col)

{- Zamienia wszystkie -1 na 0 w tablicy -}
minutsOneToZeroArr :: [Int] -> [Int]
minutsOneToZeroArr [] = []
minutsOneToZeroArr (x:xs) | x == -1 = 0:minutsOneToZeroArr xs
 | otherwise = x:minutsOneToZeroArr xs 

{- Zamienia wszystkie -1 na 0 na planszy -}
minutsOneToZeroBoard :: Board -> [[Int]]
minutsOneToZeroBoard [] = []
minutsOneToZeroBoard (x:xs) = (minutsOneToZeroArr x):minutsOneToZeroBoard xs

{- Wypełnienie planszy -}
newMap :: Size -> Int -> Board
newMap (n1,n2) v = replicate n1 (replicate n2 v)

{- Wypełnianie map dla różnych wyników iloczynu kartezjańskiego -}
mergeAllMaps :: Board -> Signs -> [Perms] -> [Board]
mergeAllMaps _ _ [] = []
mergeAllMaps clearMx signs (perms:restPerms) = (mergeMap clearMx signs perms) : (mergeAllMaps clearMx signs restPerms)

{- Wypełnianie planszy wynikiem iloczynu kartezjańskiego -}
mergeMap :: Board -> Signs -> Perms -> Board 
mergeMap mx [] [] = mx
mergeMap mx ((pos,_):signs) (perm:perms) = mergeMap nmx signs perms
                                          where nmx = mergeOnePermToMap mx pos perm

{- Dodanie do mapy permutacji dla danego oznaczenia-}
mergeOnePermToMap :: Board -> (Int, Int) -> [Int] -> Board
mergeOnePermToMap mx _ [] = mx
mergeOnePermToMap mx c (p:perm) | isCordsInsideMap mx cords = mergeOnePermToMap nmx c perm
                                | otherwise = mergeOnePermToMap mx c perm
                                where n = 3 - (length perm)
                                      cords = cordsForPermPos n c
                                      nmx = updateBoard mx p cords
{-
Wyliczenie koordynatów dla danego oznaczenia
Pierwszy argument oznacza położenie względem oznaczenia,
które należy wyliczyć:
0|1
2|3
-}
cordsForPermPos :: Int -> (Int, Int) -> (Int, Int)
cordsForPermPos n (y,x) | n == 0 = (y-1, x-1)
                        | n == 1 = (y-1, x)
                        | n == 2 = (y, x-1)
                        | n == 3 = (y, x)

{- Sprawdzenie czy koordynaty wskazują na wnętrze mapy-}
isCordsInsideMap :: Board -> (Int, Int) -> Bool
isCordsInsideMap mx (x, y) | (x >= 0) && (x < nx) && (y >= 0) && (y < ny) = True
                           | otherwise = False
                           where nx = length mx
                                 ny = length $ head mx

{- Aktualizacja macierzy o podaną wartość na podanej pozycji -}
updateBoard :: [[a]] -> a -> (Int, Int) -> [[a]]
updateBoard m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

{- Zamienia macierz Int w listę Int -}
convMxToList :: Board -> [Int]
convMxToList mx = [x | y <- mx, x <- y]

{- Wyswietlenie wszystkich map z listy -}
printAllMaps :: [Board] -> IO ()
printAllMaps [] = return ()
printAllMaps (f:l) = do printMap f
                        putStrLn ""
                        printAllMaps l

{- Wyświetlanie mapy -}
printMap :: Board -> IO ()
printMap [] = return ()
printMap (f:mx) = do printRow f 
                     printMap mx

{- Wyświetlanie wiersza -}
printRow :: [Int] -> IO ()
printRow l = putStrLn (convIntsToStrInMx l)

{-
Funkcja pomocnicza - konwertuje tablicę Intów do łańcucha znaków
-}
convIntsToStrInMx :: [Int] -> String
convIntsToStrInMx [] = []
convIntsToStrInMx is = [intToCharInMx i | i <- is]

{-
Funkcja pomocnicza - zamienia znaki z macierzy wynikowej na dane wyświetlane,
gdzie 0 to 'O' oraz 1 to 'X'
-}
intToCharInMx :: Int -> Char
intToCharInMx i = case i of
                           0    -> 'O'
                           1    -> 'X'
                           (-1) -> '?'

{-
Poniższe funkcje służą do generowania unikalnych permutacji
Źródło: http://codemirror.net/mode/haskell/
-}

-- | Find all unique permutations of a list where there might be duplicates.
uniquePerms :: (Eq a) => [a] -> [[a]]
uniquePerms = permBag . makeBag

-- | An unordered collection where duplicate values are allowed,
-- but represented with a single value and a count.
type Bag a = [(a, Int)]

makeBag :: (Eq a) => [a] -> Bag a
makeBag [] = []
makeBag (a:as) = mix a $ makeBag as
  where
    mix a []                        = [(a,1)]
    mix a (bn@(b,n):bs) | a == b    = (b,n+1):bs
                        | otherwise = bn : mix a bs

permBag :: Bag a -> [[a]]
permBag [] = [[]]
permBag bs = concatMap (\(f,cs) -> map (f:) $ permBag cs) . oneOfEach $ bs
  where
    oneOfEach [] = []
    oneOfEach (an@(a,n):bs) =
        let bs' = if n == 1 then bs else (a,n-1):bs
        in (a,bs') : mapSnd (an:) (oneOfEach bs)
    
    apSnd f (a,b) = (a, f b)
    mapSnd = map . apSnd