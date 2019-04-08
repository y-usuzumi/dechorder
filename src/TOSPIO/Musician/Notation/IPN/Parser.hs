module TOSPIO.Musician.Notation.IPN.Parser ( pitchParser
                                           , intervalParser
                                           , pp, ip
                                           ) where

import           Data.Maybe
import           Data.Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           TOSPIO.Musician.Notation.IPN

data Quality = Diminished | Minor | Perfect | Major | Augmented

type Parser = Parsec Void Text

degreeParser :: Parser Degree
degreeParser = read . (:[]) <$> oneOf ("CDEFGAB" :: String)

pitchSymbolParser :: Parser Symbol
pitchSymbolParser = do
  ch <- oneOf ("#b" ::  String) <|> return 'n'
  return $ case ch of
    '#' -> Sharp
    'b' -> Flat
    'n' -> Natural

octaveParser :: Parser Int
octaveParser = decimal

pitchParser :: Parser Pitch
pitchParser = do
  deg <- degreeParser
  symbol <- pitchSymbolParser
  octave <- octaveParser
  let symbols = case symbol of
                  Sharp   -> [Sharp]
                  Flat    -> [Flat]
                  Natural -> []
  return $ Pitch (octave, PitchClass (deg, symbols))

intervalParser :: Parser Interval
intervalParser = do
  number <- subtract 1 <$> decimal
  ch <- oneOf ("Mm+°" :: String) <|> return '.'
  let quality = case ch of
                  'M' -> 0
                  'm' -> -1
                  '+' -> 1
                  '°' -> if number == 0 || number == 3 || number == 4 || number == 7 then -1 else -2
                  '.' -> 0

  return Interval { number
                  , quality
                  }

pp :: Text -> Pitch
pp = fromJust . parseMaybe pitchParser

ip :: Text -> Interval
ip = fromJust . parseMaybe intervalParser
