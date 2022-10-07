{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Text.Megaparsec
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types (runTypeChecker, TypeError (TypeError), ColumnTypeProvider)
import Control.Monad (forM_)
import qualified Data.Map as M
-- import Conversion

data DBThing = DBThing

instance ColumnTypeProvider DBThing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Hello Pilpilang!"
            contents <- T.pack <$> getContents
            parseTest (pAssoc <* eof) contents
        [path] -> do
            contents <- TIO.readFile path
            parseTest (pAST <* eof) contents
            let Right ast = parse (pAST <* eof) "" contents
            case runTypeChecker DBThing ast of
                Right (_, preds) -> do
                    putStrLn "type checked correctly:"
                    let preds' = M.toList preds
                    forM_ preds' print
                Left (TypeError t) -> TIO.putStrLn t
        _ -> putStr "invalid"
