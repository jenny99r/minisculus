
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List

alpha = [
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  '.', ',', '?', '!', '\'', '"', ' ']

alphaCycle =
  cycle alpha

decode toDecode wheelIndex1 wheelIndex2 =
  decodeWheel3 [] toDecode (decode1And2 wheelIndex1 wheelIndex2)

--decode1And2 :: Int -> Int -> [Char]
decode1And2 wheelIndex1 wheelIndex2 charToDecode =
  (Map.fromList (encodeAlphaWheel1And2 wheelIndex1 wheelIndex2)) ! charToDecode

--decodeWheel3 :: [Char] -> ([Char] -> [Char])
decodeWheel3 d [] _ = reverse d
decodeWheel3 [] (e:es) fn = decodeWheel3 ((fn e):[]) es fn
decodeWheel3 (d:ds) (e:es) fn = decodeWheel3 ((fn (undoWheel3 d e)):(d:ds)) es fn

undoWheel3 :: Char -> Char -> Char
undoWheel3 previous encodedChar =
  alphaCycle !! (((indexOfChar encodedChar) + (3 * length alpha)) - (2 * (indexOfChar previous)))


indexOfChar char =
  case elemIndex char alphaCycle of
    Just n -> n
    Nothing -> -1

encodeAlphaWheel1And2 wheelIndex1 wheelIndex2 =
  zip (doWheel2 wheelIndex2 (doWheel1 wheelIndex1 alpha)) alpha

doWheel1 wheelIndex toEncode =
  map (wheel1 wheelIndex) toEncode

doWheel2 wheelIndex toEncode =
  map (wheel2 wheelIndex) toEncode

wheel1 wheelIndex charToEncode =
  applyWheel charToEncode (\n -> alphaCycle !! (n + wheelIndex))

wheel2 wheelIndex charToEncode =
  applyWheel charToEncode (\n -> alphaCycle !! (length alpha + n - (2 * wheelIndex)))

applyWheel charToEncode foundFn =
  case elemIndex charToEncode alphaCycle of
    Just n -> foundFn n
    Nothing -> 'h'

