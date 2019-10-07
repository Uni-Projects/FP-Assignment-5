module Numeral
where
import Data.List

power :: Base -> Digit -> Digit -> Integer
power base x y = base * x + y

type Base   =  Integer
type Digit  =  Integer

msdf, lsdf :: Base -> [Digit] -> Integer

msdf b xs = foldl (\acc el -> power b acc el ) 0 xs

lsdf b xs = foldr (\el acc -> power b acc el ) 0 xs
