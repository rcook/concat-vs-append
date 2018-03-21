{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (replicateM)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Foldable (foldr', toList)
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq (fromList)
import Data.Traversable (forM)
import Test.RandomStrings (randomASCII, randomWord)

-- Naive implementation
joinWordsAppend :: [String] -> String
joinWordsAppend (w : ws) = w ++ joinWordsAppend ws
joinWordsAppend _ = ""

-- Concat-based implementation
joinWordsConcat :: [String] -> String
joinWordsConcat = concat

joinWordsShowS :: [String] -> String
joinWordsShowS xs = foldr' (.) id (map showString xs) $ ""

joinWordsSeq :: [String] -> String
joinWordsSeq = toList . go
    where
        go :: [String] -> Seq Char
        go (w : ws) = Seq.fromList w >< go ws
        go _ = Empty

ns :: [Int]
ns = [1, 2, 5, 10, 20, 50]

stringLength :: Int
stringLength = 10000

main :: IO ()
main = do
    !examples <- zip ns <$> forM ns (flip replicateM $ randomWord randomASCII stringLength)
    let fs =
            [ ("joinWordsAppend", joinWordsAppend)
            , ("joinWordsConcat", joinWordsConcat)
            , ("joinWordsShowS", joinWordsShowS)
            , ("joinWordsSeq", joinWordsSeq)
            ]
    defaultMain $
        map (\(name, f) ->
            bgroup name (map (\(n, ws) -> bench (show n) $ nf f ws) examples)) fs
