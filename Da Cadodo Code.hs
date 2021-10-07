-- Welcome to Da Cadodo Code
--
-- INSTRUCTIONS
-- 
-- Pick whatever book you want (the receiver of the message must have the same exact copy!!).
--
-- Note the page, line and position on the line for each word.
--
-- For example the word Beethoven appears on page 51, line 15, position 8 on "Biliões e Biliões" by Carl Sagan.
-- Hence the input we insert into the code will be a tuple like this (51,15,8).
-- 
-- Make a list like this [(51,15,8),(49,8,6),(91,16,6)] with as many words as you'd like.
--
-- Go to the bottom where it says print and write (daCadodoCode [(51,15,8),(49,8,6),(91,16,6)]) inside it.
--
-- Wait a few seconds and the code is done.
--
-- You'll get something like this "wmmana..,,sfnwni..,smmsnf..,,".
--
-- To reverse the code it's more tricky because I'm a weak programmer.
--
-- Go to the bottom where it says print and write (daCadodoCodeReverse "wmmana..,,sfnwni..,smmsnf..,,") inside it.
--
-- Wait a few seconds.
--
-- You'll get something like this "511508490806911606..,,..,..,,".
--
-- It's weird but if you look closely you can notice some similarities with the initial tuple.
--
-- Each dot represents the number of algarism of the page and the comas the number of algarisms of the line (ignore the zeros, they are just there to help to translate). 
--
-- If you translate this you'll get (51,15,08),(49,8,6),(91,16,6).
--
-- Congratulations! You've successfully used the Da Cadodo Code in both ways. Now pick up a book you and a friend have and start communicating in secret!



-- -----------------------------------------------------------
-- -----------------------------------------------------------
-- -----------------------------------------------------------

-- daCadodoCode (all the code lines for it)

tuploEmLista :: (Int,Int,Int) -> [Int]
tuploEmLista (x,y,z) = [div x 10] ++ [mod x 10] ++ [div y 10] ++ [mod y 10] ++ [div z 10] ++ [mod z 10]

filterZeros :: [Int] -> [Int]
filterZeros l = filter (/= 0) l

tenMinus :: [Int] -> [Int]
tenMinus [] = []
tenMinus (h:t) = if h == 0
                 then h : tenMinus t
                 else n : tenMinus t
                    where n = 10 - h

inverter :: [Int] -> [Int]
inverter [] = []
inverter (h:t) = last t : aux2 t ++ [h] 
                   where aux2 :: [a] -> [a]
                         aux2 [x] = []
                         aux2 (h:t) = h : aux2 t 


-- "From what they say and I remember of my natural..." from the book "Meditations" by Marcus Aurelius Book One -> 2
--  1    2    3    4   5   6 7        8  9  0

meditations :: [Int] -> [Char]
meditations [] = []
meditations (h:t) | h == 0 = 'n' : meditations t
                  | h == 1 = 'f' : meditations t 
                  | h == 2 = 'w' : meditations t 
                  | h == 3 = 't' : meditations t 
                  | h == 4 = 's' : meditations t 
                  | h == 5 = 'a' : meditations t 
                  | h == 6 = 'i' : meditations t 
                  | h == 7 = 'r' : meditations t 
                  | h == 8 = 'o' : meditations t 
                  | h == 9 = 'm' : meditations t 

paginaEmStr :: (Int,Int,Int) -> [Int]
paginaEmStr (x,_,_) = [div x 10, mod x 10]

linhaEmStr :: (Int,Int,Int) -> [Int]
linhaEmStr (_,x,_) = [div x 10, mod x 10]

dots :: [Int] -> [Char]
dots [] = []
dots (h:t) = '.' : dots t

comas :: [Int] -> [Char]
comas [] = []
comas (h:t) = ',' : comas t 

juntaTudo :: [a] -> [a] -> [a] -> [a]
juntaTudo l1 l2 l3 = l1 ++ l2 ++ l3

daCadodoCode :: [(Int,Int,Int)] -> String 
daCadodoCode [] = ""
daCadodoCode (h:t) = (juntaTudo l1 l2 l3) ++ daCadodoCode t
                        where l1 = meditations (inverter (tenMinus (tuploEmLista h)))
                              l2 = dots (filterZeros (paginaEmStr h))
                              l3 = comas (filterZeros (linhaEmStr h))

-- -----------------------------------------------------------
-- -----------------------------------------------------------
-- -----------------------------------------------------------

-- daCadodoCodeReverse (all the code lines for it)

filterLetters :: String -> [Char]
filterLetters [] = []
filterLetters (h:t) | h == 'f' = h : filterLetters t
                    | h == 'w' = h : filterLetters t
                    | h == 't' = h : filterLetters t
                    | h == 's' = h : filterLetters t
                    | h == 'a' = h : filterLetters t
                    | h == 'i' = h : filterLetters t
                    | h == 'r' = h : filterLetters t
                    | h == 'o' = h : filterLetters t
                    | h == 'm' = h : filterLetters t
                    | h == 'n' = h : filterLetters t
                    | otherwise = filterLetters t

filterSymbols :: String -> [Char]
filterSymbols [] = []
filterSymbols (h:t) | h == '.' = h : filterSymbols t
                    | h == ',' = h : filterSymbols t 
                    | otherwise = filterSymbols t

turnLettersInNumbers :: [Char] -> [Int]
turnLettersInNumbers [] = []
turnLettersInNumbers (h:t) | h == 'f' = 1 : turnLettersInNumbers t
                           | h == 'w' = 2 : turnLettersInNumbers t
                           | h == 't' = 3 : turnLettersInNumbers t
                           | h == 's' = 4 : turnLettersInNumbers t
                           | h == 'a' = 5 : turnLettersInNumbers t
                           | h == 'i' = 6 : turnLettersInNumbers t
                           | h == 'r' = 7 : turnLettersInNumbers t
                           | h == 'o' = 8 : turnLettersInNumbers t
                           | h == 'm' = 9 : turnLettersInNumbers t
                           | h == 'n' = 0 : turnLettersInNumbers t

divideNumbers :: [Int] -> [[Int]]
divideNumbers [] = []
divideNumbers l = take 6 l : divideNumbers (drop 6 l)

tenMinusAgain :: [Int] -> [Int]
tenMinusAgain [] = []
tenMinusAgain (h:t) = if h == 0
                      then h : tenMinusAgain t
                      else n : tenMinusAgain t
                         where n = 10 - h

inverter2 :: [[Int]] -> [Int]
inverter2 [] = []
inverter2 (h:t) = inverter h ++ inverter2 t

listIntToDigit :: [Int] -> [Char]
listIntToDigit l = map intToDigit l

intToDigit :: Int -> Char
intToDigit i | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
             | i >= 10 && i <= 35   =  toEnum (fromEnum 'a' + i - 10)
             | otherwise            =  error "Char.intToDigit: not a digit"

daCadodoCodeReverse :: String -> [Char]
daCadodoCodeReverse [] = []
daCadodoCodeReverse l = l1 ++ l2
                        where l1 = listIntToDigit (inverter2 (divideNumbers (tenMinusAgain (turnLettersInNumbers (filterLetters l)))))
                              l2 = filterSymbols l



-- -----------------------------------------------------------
-- -----------------------------------------------------------
-- -----------------------------------------------------------

-- Here is the starting point

main :: IO ()
main =  do

print()
-- Write (daCadodoCode code) or (daCadodoCodeReverse code) inside the print and watch the magic!
