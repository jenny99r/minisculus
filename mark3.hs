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


encode toEncode wheelIndex1 wheelIndex2 =
  doWheel3 toEncode (doWheel2 wheelIndex2 (doWheel1 wheelIndex1 toEncode))

doWheel1 wheelIndex toEncode =
  map (wheel1 wheelIndex) toEncode

doWheel2 wheelIndex toEncode =
  map (wheel2 wheelIndex) toEncode

doWheel3 original toEncode =
  map wheel3 (zip toEncode ((alpha !! 0):original))

wheel1 wheelIndex charToEncode =
  applyWheel charToEncode (\n -> alphaCycle !! (n + wheelIndex))

wheel2 wheelIndex charToEncode =
  applyWheel charToEncode (\n -> alphaCycle !! (length alpha + n - (2 * wheelIndex)))

wheel3 encodeTuple =
  applyWheel (fst encodeTuple) (\n -> alphaCycle !! (n + 2 * (indexOfChar (snd encodeTuple))))

applyWheel charToEncode foundFn =
  case elemIndex charToEncode alphaCycle of
    Just n -> foundFn n
    Nothing -> 'h'

indexOfChar char =
  case elemIndex char alphaCycle of
    Just n -> n
    Nothing -> -1
