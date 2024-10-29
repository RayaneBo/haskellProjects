-- creating :
-- the tree with the morse code
-- the function that read morse code
-- the function that read morse code (including Nothing)
-- the function that write the result in a file

data Tree a = N a (Tree a) (Tree a) | L
    deriving Show

data MorseChar = Point | Trait | Pause
    deriving Show

type MorseCode = [Maybe MorseChar]

arbreS :: Tree Char
arbreS = (N 's' (N 'h' L L) 
                (N 'v' L L))

arbreU :: Tree Char
arbreU = (N 'u' (N 'f' L L) 
                L)

arbreR :: Tree Char
arbreR = (N 'r' (N 'l' L L) 
                L)

arbreW :: Tree Char
arbreW = (N 'w' (N 'p' L L) 
                (N 'j' L L))

arbreD :: Tree Char
arbreD = (N 'd' (N 'b' L L) 
                (N 'x' L L))

arbreK :: Tree Char
arbreK = (N 'k' (N 'c' L L) 
                (N 'y' L L))

arbreG :: Tree Char
arbreG = (N 'g' (N 'z' L L) 
                (N 'q' L L))

arbreO :: Tree Char
arbreO = (N 'o' L L)

morseTree :: Tree Char
morseTree = (N '' (N 'e' (N 'i' arbreS arbreU) 
                            (N 'a' arbreR arbreW)) 
                    (N 't' (N 'n' arbreD arbreK) 
                            (N 'm' arbreG arbreO)))


charToMorseChar :: Char -> Maybe MorseChar
charToMorseChar '.' = Just Point
charToMorseChar '_' = Just Trait
charToMorseChar ' ' = Just Pause
charToMorseChar _ = Nothing

strToMorseChar :: String -> MorseCode
strToMorseChar [] = []
strToMorseChar (x:xs) = charToMorseChar x : strToMorseChar xs

morseTreeDecode :: MorseCode -> Tree Char -> String
morseTreeDecode [] _ = []
morseTreeDecode _ L = []
morseTreeDecode (Just Point:xs) (N c g d) = morseTreeDecode xs g
morseTreeDecode (Just Trait:xs) (N c g d) = morseTreeDecode xs d
morseTreeDecode (Just Pause:xs) (N c g d) = c : morseTreeDecode xs morseTree

decodeMorse :: String -> String
decodeMorse [] = []
decodeMorse s = morseTreeDecode (strToMorseChar s) morseTree

decodeMorseFile :: FilePath -> IO String
decodeMorseFile f = fmap decodeMorse (readFile f)
