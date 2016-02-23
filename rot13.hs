import Data.Char (chr, ord, isAlpha, isUpper)

validateNewChar newChar z = if newChar > z then newChar - 26 else newChar

add13 x = x + 13

upperLimit c = if isUpper c then 90 else 122

rot13 c =
  let z = upperLimit c
      newChar = add13 (ord c)
      validChar = validateNewChar newChar z
      c' = chr validChar
  in if isAlpha c then c' else c

rotString13 s = map rot13 s
