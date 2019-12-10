{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

deriving instance Show MScore
deriving instance Show Score
deriving instance Show InfoOut
deriving instance Show Info
deriving instance Show Meta
deriving instance Show Part
deriving instance Show Staff
deriving instance Show Instrument
deriving instance Show Artic
deriving instance Show Channel
deriving instance Show Music
deriving instance Show VBox
deriving instance Show Text
deriving instance Show Measure
deriving instance Show Chord
deriving instance Show DurationT
deriving instance Show Note
deriving instance Show Scale
deriving instance Show RNum

data MScore = MScore String String [Info] Score

parseMScore :: Parser MScore
parseMScore = MScore <$> parseHeader <*> parseHeader <*> many (try parseInfo) <*> parseScore <* endBracket

data Score = Score [InfoOut] [Meta] Part Music

parseScore :: Parser Score
parseScore = Score <$ tok "<Score>" <*> many (try parseInfoOut) <*> many (try parseMeta) <*> parsePart <*> parseMusic <* endBracket

data InfoOut = InfoPiece Info | Style [Info]

parseInfoOut :: Parser InfoOut
parseInfoOut = try (InfoPiece <$> parseInfo)
              <|> try (InfoPiece <$> (Info <$ tok "<" <*> alphaIncQuote <* tok ">" <*> alpha <* endBracket))
              <|> Style <$ tok "<Style>" <*> many (try parseInfo) <* endBracket

data Info = Info String String

parseInfo :: Parser Info
parseInfo = Info <$ tok "<" <*> alpha <* tok ">" <*> alpha <* endBracket

data Meta = Meta String String

parseMeta :: Parser Meta
parseMeta = Meta <$ tok "<metaTag name=\"" <*> alpha <* tok "\">" <*> alpha <* endBracket

data Part = Part Staff Info Instrument

parsePart :: Parser Part
parsePart = Part <$ tok "<Part>" <*> parseStaff <*> parseInfo <*> parseIns <* tok "</Part>"

data Staff = Staff Integer String String

parseStaff :: Parser Staff
parseStaff = Staff <$ tok "<Staff id=\"" <*> number <* tok "\">" <* tok "<StaffType group=\"" <*> alpha <* tok "\">" <* tok "<name>" <*> alpha <* (endBracket) <* endBracket <* endBracket

data Instrument = Instrument [Info] [Artic] Channel

parseIns :: Parser Instrument
parseIns = Instrument <$ tok "<Instrument>" <*> many (try parseInfo) <*> many (try parseArtic) <*> parseChannel <* endBracket

data Artic = Artic Integer Integer | ArticNamed String Integer Integer

parseArtic :: Parser Artic
parseArtic = try (Artic <$ tok "<Articulation>" <* tok "<velocity>" <*> number <* endBracket <* tok "<gateTime>" <*> number <* endBracket <* endBracket)
            <|> (ArticNamed <$ tok "<Articulation name=\"" <*> alpha <* tok "\">" <* tok "<velocity>" <*> number <* endBracket <* tok "<gateTime>" <*> number <* endBracket <* endBracket)

data Channel = Channel Integer [Info]

parseChannel :: Parser Channel
parseChannel = Channel <$ tok "<Channel>" <* tok "<program value=\"" <*> number <* tok "\"/>" <*> many (try parseInfo) <* endBracket

data Music = Music Integer VBox [Measure]

parseMusic :: Parser Music
parseMusic = Music <$> (parseQuote "Staff id" >>= return . read)  <*> parseVBox <*> many (try parseMeasure) <* endBracket

data VBox = VBox Info [Text]

parseVBox :: Parser VBox
parseVBox = VBox <$ tok "<VBox>" <*> parseInfo <*> many (try parseText) <* endBracket

data Text = Text [Info]

parseText :: Parser Text
parseText = Text <$ tok "<Text>" <*> many (try (parseInfo)) <* endBracket

data Measure = Measure [Chord] | MeasureTSig Integer Integer [Chord]

parseMeasure :: Parser Measure
parseMeasure = try (MeasureTSig <$ tok "<Measure>" <* tok "<voice>"
                  <* tok "<TimeSig>" <* tok "<sigN>" <*> number <* tok "</sigN>" <* tok "<sigD>" <*> number <* tok "</sigD>" <* tok "</TimeSig>"
                  <*> many parseChord <* endBracket <* endBracket)
              <|> Measure <$ tok "<Measure>" <* tok "<voice>" <*> many parseChord <* endBracket <* endBracket

data Chord = Chord DurationT [Note] | Rest DurationT String

parseChord :: Parser Chord
parseChord = try (Chord <$ tok "<Chord>" <* tok "<durationType>" <*> parseDT <* endBracket <*> many parseNote <* endBracket)
            <|> Rest <$ tok "<Rest>" <* tok "<durationType>" <*> parseDT <* endBracket <* tok "<duration>" <*> alphaIncSlash <* endBracket <* endBracket

data DurationT = Dur String

parseDT :: Parser DurationT
parseDT = Dur <$> alpha

data Note = Note Integer Integer

parseNote :: Parser Note
parseNote = Note <$ tok "<Note>" <* tok "<pitch>" <*> number <* endBracket <* tok "<tpc>" <*> number <* endBracket <* endBracket

parseFile :: String -> Maybe MScore
parseFile s = case runParser parseMScore "ERROR" s of
                Left err -> Nothing
                Right p -> Just p

parseAny :: String -> Parser a -> Maybe a
parseAny s a = case runParser a "ERROR" s of
                Left err -> Nothing
                Right p -> Just p

tok :: String -> Parser String
tok t = whitespace *> string t <* whitespace

parseFromFile :: Show a => Parser a -> FilePath -> IO ()
parseFromFile prog filePath = do
  file <- readFile filePath
  putStrLn $ case parse prog filePath file of
    Left err   -> show err
    Right prog -> show prog

--compileFile :: FilePath -> IO ()
--compileFile filePath = do
--  file <- readFile filePath
--  putStrLn $ case parse prog filePath file of
--    Left err   -> parseErrorPretty err
--    Right prog -> show (compileProg2MC prog)

whitespace :: Parser ()
whitespace = many (oneOf " \t\n") *> pure ()

number :: Parser Integer
number = whitespace *> (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

alpha :: Parser String
alpha = many (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " .-"))

alphaIncQuote :: Parser String
alphaIncQuote = many (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " .=\""))

alphaIncSlash :: Parser String
alphaIncSlash = many (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " ./"))

parseHeader :: Parser String
parseHeader = tok "<" *> some (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "?=-+$&^ ./\"")) <* tok ">"

endBracket :: Parser String
endBracket = tok "</" *> alpha <* tok ">"

parseQuote :: String -> Parser String
parseQuote s = tok ("<" ++ s ++ "=\"") *> alpha <* tok "\">"

testNote    = "<Note>  <pitch>77</pitch>  <tpc>13</tpc>  </Note>"
testDT      = "32nd"
testChord   = "<Chord>  <durationType>half</durationType>  <Note>    <pitch>76</pitch>    <tpc>18</tpc>    </Note>  </Chord>"
testRest    = "<Rest>            <durationType>measure</durationType>            <duration>4/4</duration>            </Rest>"
testMeasure = "<Measure>  <voice>    <Chord>      <durationType>half</durationType>      <Note>        <pitch>76</pitch>        <tpc>18</tpc>        </Note>      </Chord>    <Chord>      <durationType>half</durationType>      <Note>        <pitch>77</pitch>        <tpc>13</tpc>        </Note>      </Chord>    </voice>  </Measure>"
testInfo    = "<style>Title Fun</style>"
testMnyInfo = "<style>Title</style> <text>Parse Test</text>"
testText    = "<Text> <style>Title</style> <text>Parse Test</text> </Text>"
testVBox    = "<VBox>  <height>10</height>  <Text>    <style>Title</style>    <text>Parse Test</text>    </Text>  <Text>    <style>Subtitle</style>    <text>Subtitle</text>    </Text>  <Text>    <style>Composer</style>    <text>Robin Chubby</text>    </Text>  <Text>    <style>Lyricist</style>    <text>Sonny Vince</text>    </Text>  </VBox>"

testFile :: String -> FilePath
testFile s = ("test/test" ++ s ++ ".txt")

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty [] = ""
  pretty (a:as) = pretty a ++ pretty as

data Scale = Scale RNum Integer Int | SubScale RNum Integer Int

instance Pretty Scale where
  pretty (Scale R a b) = take b (repeat ('.'))
  pretty (Scale r a b) = "(" ++ pretty r ++ ")" ++ (take (b - 1) (repeat '-'))
--           t   sd    t    sd   d   t     d
data RNum  = I | II | III | IV | V | VI | VII | R

instance Pretty RNum where
  pretty R = "."
  pretty s = show s
