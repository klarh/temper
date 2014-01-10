{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Control.Applicative((<$>))
import Control.Monad.State.Lazy
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Monoid (mconcat)
import Data.Ord (Ordering(..), compare)
import Data.Text.Lazy (Text(..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IT (readFile, putStrLn)
import Data.Text.Lazy.Builder (Builder(..), fromLazyText, toLazyText)
import System.Console.CmdArgs
import System.FilePath.Posix(joinPath)
import System.IO (FilePath(..))
import Text.Temper
type ReplaceTable = M.Map Char T.Text

baseTable::ReplaceTable
baseTable = M.fromList [('"', "&quot;"),
                        ('\'', "&apos;"),
                        ('&', "&amp;"),
                        ('<', "&lt;"),
                        ('>', "&gt;")]

data Options = Options {inFile :: String,
                        outFile :: String} deriving (Data, Typeable, Show)

defopts = Options {inFile = def &= typFile,
                   outFile = def}

encodeHtml = T.concatMap (\c -> M.findWithDefault (T.singleton c) c baseTable)

type Parser a = State (M.Map Int Text) a

emptyParser = M.empty

closeTop::Parser Text
closeTop = do
  tags <- get

  case M.null tags of
    False -> do
      let ((_, top), rest) = M.deleteFindMax tags
      put rest
      return ('<' `T.cons` '/' `T.cons` top `T.snoc` '>')
    _ -> return T.empty

pushStack::Int->Text->Parser ()
pushStack indent new = modify (M.insert indent new)

expandAttributes::Text->Text
expandAttributes name = T.concat [elt, ids, classes]
  where
    (elt:segments') = concat $ T.splitOn "#" <$> T.splitOn "." name
    controllers = T.unpack $ T.filter (\c -> c == '#' || c == '.') name
    segments = zip controllers segments'
    ids' = filter ((== '#') . fst) segments
    ids
      | null ids' = T.empty
      | otherwise = " id=\"" `T.append` (snd . head $ ids') `T.snoc` '"'
    classes' = filter ((== '.') . fst) segments
    classes
      | null classes' = T.empty
      | otherwise = " class=\"" `T.append` (T.unwords $ snd <$> classes') `T.snoc` '"'

makeTag::Text->Parser Text
makeTag x = do
  let closeImmediately = (T.last x == '/') || (T.head x == '!')
      (eltName:rest) = T.words x
      eltName' = expandAttributes eltName

  when closeImmediately $ do
    _ <- closeTop
    return ()
  return ('<' `T.cons` eltName' `T.append` T.unwords rest `T.snoc` '>')

cleanTag::Text->Text
cleanTag = stripShortcuts . head . T.words
  where
    stripShortcuts = T.takeWhile (\c -> c /= '.' && c /= '#')

indentationLevel::Text->Int
indentationLevel = fromIntegral . T.length . T.takeWhile isSpace

increaseIndentation::Int->Text->Parser Text
increaseIndentation indent new = do
  pushStack indent (cleanTag new)
  makeTag new

sameIndentation::Int->Text->Parser Text
sameIndentation indent new = do
  top <- closeTop
  pushStack indent new
  return top

decreaseIndentation::Int->Text->Parser Text
decreaseIndentation indent new = do
  tags <- get
  let (toClose, remainder) = M.partitionWithKey (\ind _ -> ind > indent) tags
  put remainder
  closed <- mconcat <$> mapM (\_ -> closeTop) (M.toDescList toClose)
  return closed

cleanParser::Parser Text
cleanParser = do
  tags <- get
  closed <- mconcat <$> mapM (\_ -> closeTop) (M.toDescList tags)
  put M.empty
  return closed

parseLine::Text->Parser Text
parseLine line = do
  tags <- get
  let lastIndent = case M.null tags of
        False -> fst . M.findMax $ tags
        _ -> 0
      indent = indentationLevel line
      line' = T.strip line

  case (compare indent lastIndent) of
    LT -> decreaseIndentation indent line'
    EQ -> sameIndentation indent line'
    GT -> increaseIndentation indent line'

parseLines::[Text]->Text
parseLines ts = mconcat starts `T.append` rest
  where
    (starts, st) = runState (mapM parseLine ts) emptyParser
    rest = evalState cleanParser st

parseFile::Text->Text
parseFile = parseLines . T.lines . encodeHtml

main = do
  (Options inFile outFile) <- cmdArgs defopts

  cs <- IT.readFile inFile
  IT.putStrLn $ parseFile cs

  return ()
