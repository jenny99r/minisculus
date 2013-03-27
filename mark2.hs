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

mark1 toEncode wheelIndex1 wheelIndex2 =
  map (wheel2 wheelIndex2) (map (wheel1 wheelIndex1) toEncode)

wheel1 wheelIndex charToEncode =
  case elemIndex charToEncode alphaCycle of
    Just n  -> alphaCycle !! (n + wheelIndex)
    Nothing -> 'h'

wheel2 wheelIndex charToEncode =
  case elemIndex charToEncode alphaCycle of
    Just n  -> alphaCycle !! (length alpha + n - (2 * wheelIndex))
    Nothing -> 'h'

