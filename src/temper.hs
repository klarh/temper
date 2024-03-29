{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Control.Applicative((<$>))
import Control.Monad.State.Lazy
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Monoid (mconcat)
import Data.Ord (Ordering(..), compare)
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.IO as IT (readFile, hPutStrLn)
import Data.Text.Lazy.Builder (Builder(..), fromLazyText, toLazyText)
import System.Console.CmdArgs
import System.FilePath.Posix(joinPath)
import System.IO (FilePath(..), IOMode(..), stdout, openFile, hClose)
import Text.Parsec.Prim (parse)
import Text.Temper

data Options = Options {inFile :: String,
                        outFile :: String} deriving (Data, Typeable, Show)

defopts = Options {inFile = def &= typFile,
                   outFile = def}

main = do
  (Options inFile outFile) <- cmdArgs defopts

  cs <- IT.readFile inFile

  let result = parse elements "" cs
      result' = case result of
        Left err -> error . show $ err
        Right ts -> T.concat $ encodeElement <$> ts

  target <- case outFile of
    "" -> return stdout
    f -> openFile f WriteMode

  IT.hPutStrLn target result'
  hClose target
