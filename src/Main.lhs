> {-# LANGUAGE PackageImports,DeriveDataTypeable #-}

  References<br/>
  [1] Bryan O’Sullivan, John Goerzen, and Don Stewart. Real World Haskell.
      O’Reilly Media, Inc., 1 edition, December 2008.<br/>
  [2] http://hackage.haskell.org/packages/archive/ppm/2009.5.13/doc/html/src/Codec-Image-PPM.html<br/>
  [3] http://hackage.haskell.org/packages/archive/jpeg/0.0.1/doc/html/src/Graphics-JPEG.html<br/>
  [4] http://cgi.cse.unsw.edu.au/~dons/blog/2006/12/18<br/>
  
The most essential parsing parts of the code are taken directly from chapter 10 RWH [1] which is and hopefully will be available online ([follow this link](http://book.realworldhaskell.org/read/)). In the chapter is described in very detail what has been chosen why to do when and so on. I will only repeat some of it as literal documentation here where I guess it deserves for my own confidence and will merge that with my comments. So in short: Don't expect what you find here to be a full equivalent substitute for studiing the original. The ideas for the parts to map the pixel-based format as itself (`ppmEncode` etc., in earlier versions of this document I also used a `PixelRGB` data type which, as I later found out, was very similar to the now imported `Colour8` data type) mainly came when studiing [2] and [3]. The command line arguments processing is done according to [4] - taking the most basic solution shown there. TODO Use cmdargs package or similar.

> module Main where

> import Data.Char(chr,isDigit,isSpace)
> import System.IO
> import System.Random (randomIO)
> import System.Environment (getArgs)
> import System.Exit (ExitCode(..),exitWith)
> import System.Directory (getTemporaryDirectory)
> import "mtl" Control.Monad.Reader
> import qualified "bytestring" Data.ByteString.Lazy.Char8 as L8
> import qualified "bytestring" Data.ByteString.Lazy as L
> import Control.Applicative ((<$>))
> import Data.Int (Int64)
> import Data.Word (Word8)
> import Data.List.Split (splitEvery)
> import Data.List (intersperse)
> import Data.Either.Utils (fromRight)
> import Data.Colour.Word8
> import System.Console.CmdArgs

> (~~) :: L.ByteString -> L.ByteString -> L.ByteString

> (~~) = L.append

> readBinFile :: String -> IO L.ByteString

> readBinFile f = do
>  h <- openBinaryFile f ReadMode
>  L.hGetContents h

> writeBinFile :: String -> L.ByteString -> IO ()

> writeBinFile f s = do
>  h <- openBinaryFile f WriteMode
>  L.hPutStr h s
>  hClose h

  Btw `show (length (head xss))` &#8801; `show $ length $ head xss`

  Test run with:

    (splitEvery 3 . L.unpack . unwrap . parse parseRawPPM . ppmEncode) [[Colour8 0 0 0, Colour8 1 0 0], [Colour8 0 1 0, Colour8 1 1 1]]

> ppmEncode :: [[Colour8]] -> L.ByteString

> ppmEncode xss
>  = profile ~~ columns ~~ rows ~~ colours ~~ (ppmEncode_) xss where
>    profile = L8.pack $ unlines ["P6"]
>    columns = L8.pack $ (show $ length $ head xss) ++ " "
>    rows =    L8.pack $ unlines [(show $ length xss)]
>    colours = L8.pack $ unlines ["255"]

  With the read-in matrix of rgb-pixels do the following as composed top down:

   * Make a linear list with `concat` (type is `[Colour8]`)
   * On each list element of the intermediate list call the `rgbPixel2ppmChars` operation with `map`
   * Generate a byte string from the resulting list with `L.concat`

  In short you might type-test it with (type is `L.ByteString`):

    :t L.concat $ map rgbPixel2ppmChars $ concat [[(Colour8 0 0 0)]]

> ppmEncode_ :: [[Colour8]] -> L.ByteString

> ppmEncode_ xss
>   = (L.concat . map rgbPixel2ppmChars . concat) xss

  Test run with `rgbPixel2ppmChars $ Colour8 255 1 255` and the result of
  `L.unpack $ rgbPixel2ppmChars $ Colour8 255 1 255` should indeed be `[255,1,255]`.

> rgbPixel2ppmChars :: Colour8 -> L.ByteString

> rgbPixel2ppmChars (Colour8 r g b)
>  = L.pack [ r, g, b ]

  ## The actual parser, highest rated approach

> newtype Parse a = Parse {
>   runParse :: ParseState -> Either String (a, ParseState)
> }

> data ParseState = ParseState {
>   string :: L.ByteString
>   , offset :: Int64           -- imported from Data.Int
> } deriving (Show)

> getState :: Parse ParseState

> getState = Parse (\s -> Right (s, s))

> putState :: ParseState -> Parse ()

> putState s = Parse (\_ -> Right ((), s))

> instance Monad Parse where
>   return = identity
>   (>>=) = (==>)
>   fail = bail

> bail :: String -> Parse a

> bail err = Parse $ \s -> Left $
>   "byte offset " ++ show (offset s) ++ ": " ++ err

  This function leaves the parse state untouched and uses its argument
  as the result of the parse.

> identity :: a -> Parse a

> identity a = Parse (\s -> Right (a, s))

  The "bind" function for the `Monad` instance.

> (==>) :: Parse a -> (a -> Parse b) -> Parse b

> firstParser ==> secondParser  =  Parse chainedParser
>   where chainedParser initState   =
>           case runParse firstParser initState of
>             Left errMessage ->
>                 Left errMessage
>             Right (firstResult, newState) ->
>                 runParse (secondParser firstResult) newState

  The function we're `fmap`ping should be applied to the current result of a parse, and
  leave the parse state untouched.

> instance Functor Parse where
>   fmap f parser = parser ==> \result ->
>     identity (f result)

  Check that identity is preserved on a parse that ought to fail: parsing a byte from an empty string
  (`(<$>)` is `fmap`): `parse parseByte L.empty`, which should have the same result as
  `parse (id <$> parseByte) L.empty`.

  How can we use this wrapped function to parse something (applies to i. e. `identity` either)?
  At first we have to peel off the `Parse` wrapper so that we can get at the function inside.
  We do so using the `runParse` function. We also need to construct a `ParseState`, and then
  run our parsing function on it. Finally we'd like to separate the result of the parse from
  the final `ParseState`.

  [manpages for ppm etc.](http://local.wasp.uwa.edu.au/~pbourke/dataformats/ppm/) tell us:<br/>
  PPM headers are P3 | P6: ascii | byte. An ascii entry has three numbers in the r-g-b order usually where
  each number usually ranges 0..255 (black..white), but thats not obligate.
  When stored as binary, the order is the same, but **one byte** is used
  **per color component**. Meaning 8 bits of information are possible for each component, makes 3x8 bits = 24 Bit
  color image storage possible with the binary format (`2^8^3` = `2^24` = `256 x 256 x 256` = `16777216`
  colors) as opposed to the ascii format where the color depth practically isn't limited to 256, because one
  may use - manpage says upper limit is - 65536 per color component entry.

  (*) See [here](http://netpbm.sourceforge.net/doc/ppm.html): "... raster of Height rows... Each row consists of Width pixels... Each pixel is a triplet of red, green, and blue samples... Each sample is represented in pure binary by either 1 or 2 bytes..." So it comes taking the number of overall bytes to be parsed (expression `width * height`) times three.

  A test run:

    (splitEvery 3 . L.unpack . unwrap . parse parseRawPPM . ppmEncode) [[Colour8 0 0 0, Colour8 1 0 0], [Colour8 0 1 0, Colour8 1 1 1]]    

> -- for PPM P6 (binary) format
> parseRawPPM :: Parse Bytemap

> parseRawPPM =
>  parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
>           assert (header == "P6") "invalid raw header" ==>&
>           parseNat ==> \width -> skipSpaces ==>&
>           parseNat ==> \height -> skipSpaces ==>&
>           parseNat ==> \maxValue ->
>           parseByte ==>&
>           -- (see (*) in the text above this function)
>           parseBytes (width * height * 3) ==> \bitmap ->
>           identity (Bytemap width height maxValue bitmap)
>     where notWhite = (`notElem` " \r\n\t")

  The function for running the `Parse` actually, base test run: `parse parseRawPPM L.empty`

> parse :: Parse a -> L.ByteString -> Either String a

> parse parser initState
>   = case runParse parser (ParseState initState 0) of
>   Left err          -> Left err
>   Right (result, _) -> Right result

> (==>&) :: Parse a -> Parse b -> Parse b

> p ==>& f = p ==> \_ -> f

> assert :: Bool -> String -> Parse ()

> assert True  _   = identity ()
> assert False err = bail err

  This "peek" function returns `Nothing` if we're at the end of the input string. Otherwise it returns
  the next character without consuming it (i. e. it inspect, but doesnt disturb the current parsing state).
  Test run: `parse peekByte L.empty`.

> peekByte :: Parse (Maybe Word8)

> peekByte = (fmap fst . L.uncons . string) <$> getState

  (Hint: Import the `Word8` type from `Data.Word`)

> parseByte :: Parse Word8

> parseByte =
>     getState ==> \initState ->
>     case L.uncons (string initState) of
>       Nothing ->
>           bail "no more input"
>       Just (byte,remainder) ->
>           putState newState ==> \_ ->
>           identity byte
>        where newState = initState { string = remainder,
>                                     offset = newOffset }
>              newOffset = offset initState + 1

  Another generic combinator, the `Parse` analogue of the familiar `takeWhile`. It consumes its input while
  its predicate returns `True`. (Its the version with no functors - a copy of `parseWhileVerbose` - likely
  easier to read if one is not common with functors)
  
> parseWhile :: (Word8 -> Bool) -> Parse [Word8]

> parseWhile p =
>          peekByte ==> \mc ->
>             case mc of
>               Nothing -> identity []
>               Just c | p c ->
>                 parseByte ==> \b ->
>                 parseWhile p ==> \bs ->
>                 identity (b:bs)
>                      | otherwise ->
>                 identity []

> parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]

> parseWhileWith f p = fmap f <$> parseWhile (p . f)

  `ByteString`s contain `Word8`s latter which are `Int` instances. Decode to the `Char` representation
  as they are in normal `String`s.

> w2c :: Word8 -> Char
> w2c = chr . fromIntegral

  Spaces Parser - active as long as spaces are decoded, simply skips them by traversing further down the
  parsing chain. Test: `parse skipSpaces L.empty`.

> skipSpaces :: Parse ()
> skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

  Parser for natural numbers - as long as `Int` values are decoded, simply return that decoded value unless it
  doesn't fulfill `Int` constraints (i. e. is negative or empty).

  Test run: `parse parseNat $ L8.pack "9"` &#8801; `parse (id <$> parseNat) $ L8.pack "9"`.
  
> parseNat :: Parse Int

> parseNat = parseWhileWith w2c isDigit ==> \digits ->
>          if null digits
>             then bail "no more input"
>          else let n = read digits
>               in if n < 0
>             then bail "integer overflow"
>          else identity n

> parseBytes :: Int -> Parse L.ByteString

> parseBytes n =
>   getState ==> \st ->
>     let n' = fromIntegral n
>         (h, t) = L.splitAt n' (string st)
>         st' = st { offset = offset st + L.length h, string = t }
>     in putState st' ==>&
>        assert (L.length h == n') "end of input" ==>&
>        identity h   

> data Bytemap = Bytemap {
>     bytesWidth :: Int
>   , bytesHeight :: Int
>   , bytesMax :: Int
>   , bytesData :: L.ByteString
> } deriving (Eq)

> instance Show Bytemap where
>   show (Bytemap w h m d) = "Bytemap " ++ show w ++ "x" ++ show h ++
>                           " " ++ show m ++ " data: " ++ (show $ L.unpack d)

> showRGBTriples :: L.ByteString -> [[Word8]]

> showRGBTriples f = do
>   (splitEvery 3 . L.unpack . unwrap) $ parse parseRawPPM f

  Function that makes a flat list of `String` separated by i. e. semicolons or what you provide (1st argument) for the RGB components from the `[[Word8]]` list:

> w82s :: [Char] -> [[Word8]] -> [Char]

> w82s c triples = do
>   concat $ intersperse c strings
>   where strings = map (show . fromEnum) $ concat triples

> unwrap :: Either String Bytemap -> L.ByteString

> unwrap bytemap = bytesData $ fromRight bytemap

  Test run: `writeSamplePPM "e:/temp/test3.ppm"`

> writeSamplePPM :: String -> IO()

> writeSamplePPM fname = do
>   L.writeFile fname $
>     ppmEncode [[Colour8 255 0 0   -- @[0,0] == ul : red pixel
>               , Colour8 0 255 0]  -- @[0,1] == ur : green pixel
>               ,[Colour8 0 0 0     -- @[1,0] == ll : black pixel
>               , Colour8 0 0 255]  -- @[1,1] == lr : blue pixel
>               ]

 When cmdargs is installed, in ghci simply use
 	:module +System.Process
	rawSystem "runhaskell" ["StratsMain.hs", "--strategy=any"]

> data Ppm2rgb = Ppm2rgb  { ifilename :: FilePath
>					, ofilename :: FilePath } deriving (Show, Data, Typeable)

> chosen = Ppm2rgb { ifilename = def &= typ "[name]" &= help "the absolute file name of the ppm file given"
>				, ofilename = def &= typ "[name|temp]" &= help "the absolute file name of the csv file (containing the ascii rgb values) to be written; saying temp here results in a file $TMP/test[RANDOM...].csv"}

> main :: IO ()
> main = do
>   cfg <- cmdArgs (modes [chosen])
>   input <- L.readFile $ ifilename cfg
>   let _ofilename_ =     ofilename cfg
>   case _ofilename_ of
>     "temp" -> do
>       r <- randomIO
>       t <- getTemporaryDirectory
>       writeFile (t ++ "test" ++ show (r + 1 :: Int) ++ ".csv") $ _bytestring2rgbliste_ input
>     _      -> writeFile _ofilename_ $ _bytestring2rgbliste_ input
>   where _bytestring2rgbliste_ = w82s ";" . showRGBTriples

