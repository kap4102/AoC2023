import Data.String
import Data.Vect
import Data.List

filterLine : String -> List Int
filterLine str = mapMaybe (parseInteger . singleton) $ filter isDigit $ unpack str     
                         
     
valuateLine : List Int -> Int
valuateLine (x :: ys@(y :: xs)) = x + last ys
valuateLine _ = 0
               
play : List String -> Int
play x = sum $ map (valuateLine . filterLine) x
play' : List String -> Int                   
play' x = foldl (+) 0 $ map (valuateLine . filterLine) x
