{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Text.Megaparsec
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types (runTypeChecker, TypeError (TypeError))
import Conversion (convertAll)

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
            -- parseTest (pAST <* eof) contents
            let Right ast = parse (pAST <* eof) "" contents
            case runTypeChecker ast of
                Right preds -> do
                    -- putStrLn "type checked correctly:"
                    mapM_ TIO.putStrLn $ convertAll preds
                Left (TypeError t) -> TIO.putStrLn t
        _ -> putStr "invalid"
