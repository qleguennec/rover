module Lib
  ( spaces',
    digits',
    comma,
  )
where

import Text.Parsec

spaces' :: (Stream s m Char) => ParsecT s u m ()
spaces' = skipMany (char ' ')

digits' :: (Stream s m Char) => ParsecT s u m Int
digits' = read <$> many1 digit

comma :: (Stream s m Char) => ParsecT s u m Char
comma = char ','

-- parseOrientation 'N' = Left $ V2 0 1
-- parseOrientation 'E' = Left $ V2 1 0
-- parseOrientation 'S' = Left $ V2 0 (-1)
-- parseOrientation 'W' = Left $ V2 (-1) 0
